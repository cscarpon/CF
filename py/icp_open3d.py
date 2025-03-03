import open3d as o3d
import numpy as np
import laspy
import os
import pyproj

class Open3DICP:
    def __init__(self, source_path, target_path, voxel_size=0.05, icp_method="point-to-point"):
        """
        Class to perform ICP alignment using Open3D while preserving LAS metadata.

        :param source_path: Path to the source point cloud (.las or .laz)
        :param target_path: Path to the target point cloud (.las or .laz)
        :param voxel_size: Downsampling voxel size (smaller means higher resolution)
        :param icp_method: "point-to-point" or "point-to-plane"
        """
        self.source_path = source_path
        self.target_path = target_path
        self.aligned_path = self._generate_aligned_path(source_path)
        self.voxel_size = voxel_size
        self.icp_method = icp_method
        self.transformation = None
        self.rmse = None
        self.metadata = None  # Store original classification/intensity/returns
        self.original_crs = None  # Store original CRS (only projected CRS)
        self.point_ids = None  # Unique ID for matching back metadata
        self.original_header = None  # Store the original LAS header

    def align(self):
        """ Performs ICP alignment, saves the aligned point cloud, and returns the aligned file path + RMSE """
        source_pcd, source_metadata, source_ids = self._load_point_cloud(self.source_path)
        target_pcd, _, _ = self._load_point_cloud(self.target_path)

        if source_pcd is None or target_pcd is None:
            print("Error: Failed to load point clouds.")
            return None  

        # Downsample point clouds
        source_down = source_pcd.voxel_down_sample(self.voxel_size)
        target_down = target_pcd.voxel_down_sample(self.voxel_size)
    
        # Estimate normals (if point-to-plane)
        if self.icp_method == "point-to-plane":
            source_down.estimate_normals(o3d.geometry.KDTreeSearchParamHybrid(radius=0.4, max_nn=30))
            target_down.estimate_normals(o3d.geometry.KDTreeSearchParamHybrid(radius=0.4, max_nn=30))
    
        # ICP Configuration
        threshold = 1.0
        trans_init = np.eye(4)
        icp_method = (
            o3d.pipelines.registration.TransformationEstimationPointToPlane()
            if self.icp_method == "point-to-plane"
            else o3d.pipelines.registration.TransformationEstimationPointToPoint()
        )
    
        try:
            # Run ICP
            result = o3d.pipelines.registration.registration_icp(
                source_down, target_down, threshold, trans_init, icp_method,
                o3d.pipelines.registration.ICPConvergenceCriteria(max_iteration=50)
            )
    
            self.transformation = result.transformation
            self.rmse = result.inlier_rmse
    
            # Apply transformation and save result
            source_pcd.transform(self.transformation)

            # Merge back metadata using IDs
            aligned_data = self._merge_metadata(source_pcd, source_metadata)

            # Save the aligned LAS file with metadata
            self._save_as_laz(aligned_data)
    
            # Format output string
            output_str = f"""
            ICP Alignment Completed.
            RMSE: {self.rmse:.6f}
            Transformation Matrix:
            {np.array_str(self.transformation, precision=6, suppress_small=True)}
            """
    
            return self.aligned_path, output_str.strip()  # Return both
    
        except Exception as e:
            return None, f"Error during ICP alignment: {str(e)}"

    def _load_point_cloud(self, file_path):
        """ Loads a LAS file into Open3D with metadata and assigns unique IDs. """
        if file_path.endswith(".las") or file_path.endswith(".laz"):
            with laspy.open(file_path) as las_file:
                las = las_file.read()
                points = np.vstack((las.x, las.y, las.z)).transpose()
                pcd = o3d.geometry.PointCloud()
                pcd.points = o3d.utility.Vector3dVector(points)

                # Assign unique point IDs
                point_ids = np.arange(len(las.x), dtype=np.int32)

                # Store classification, intensity, and return info
                metadata = {
                    "id": point_ids,  
                    "classification": las.classification.copy(),
                    "intensity": las.intensity.copy(),
                    "return_number": las.return_number.copy(),
                    "num_returns": las.num_returns.copy(),
                }

                # Store original LAS header
                self.original_header = las.header

                # Handle CRS properly
                try:
                    crs = las.header.parse_crs()
                    if crs is not None:
                        if isinstance(crs, pyproj.CRS):
                            # Check if it's a compound CRS
                            if crs.is_compound:
                                self.original_crs = crs.sub_crs_list[0]  # Use only the projected CRS
                            else:
                                self.original_crs = crs
                        else:
                            print(f"Warning: Could not parse CRS '{crs}', using default EPSG:26917")
                            self.original_crs = pyproj.CRS.from_epsg(26917)
                    else:
                        print("Warning: No CRS found, using EPSG:26917")
                        self.original_crs = pyproj.CRS.from_epsg(26917)
                except Exception:
                    self.original_crs = pyproj.CRS.from_epsg(26917)

                return pcd, metadata, point_ids
            
        elif file_path.endswith(".ply"):
            return o3d.io.read_point_cloud(file_path), None, None
        else:
            print("Unsupported file format")
            return None, None, None
    
    def _generate_aligned_path(self, source_path):
        """Generates a filename for the aligned LAS file."""
        base, _ = os.path.splitext(source_path)
        return f"{base}_aligned.laz"

    def _merge_metadata(self, transformed_pcd, metadata):
        """ Merges transformed XYZ with original metadata using IDs. """
        num_points = len(transformed_pcd.points)
        
        if num_points != len(metadata["id"]):
            print("Warning: Point count mismatch! Adjusting metadata size.")
            min_len = min(num_points, len(metadata["id"]))
            for key in metadata:
                metadata[key] = metadata[key][:min_len]

        # Create structured array for LAS output
        new_data = np.zeros(num_points, dtype=[("x", "f4"), ("y", "f4"), ("z", "f4"),
                                            ("classification", "u1"), ("intensity", "u2"),
                                            ("return_number", "u1"), ("num_returns", "u1")])

        new_data["x"] = np.asarray(transformed_pcd.points)[:, 0]
        new_data["y"] = np.asarray(transformed_pcd.points)[:, 1]
        new_data["z"] = np.asarray(transformed_pcd.points)[:, 2]
        new_data["classification"] = metadata["classification"]
        new_data["intensity"] = metadata["intensity"]
        new_data["return_number"] = metadata["return_number"]
        new_data["num_returns"] = metadata["num_returns"]

        return new_data

    def _save_as_laz(self, aligned_data):
        """ Saves transformed LAS file with original metadata. """
        header = laspy.LasHeader(point_format=3, version="1.4")
        header.offsets = np.min(np.vstack((aligned_data["x"], aligned_data["y"], aligned_data["z"])).T, axis=0)
        header.scales = np.array([0.01, 0.01, 0.01])  

        if self.original_crs is not None:
            try:
                header.add_crs(self.original_crs)
            except Exception as e:
                print(f"Warning: Failed to apply original CRS. Assigning EPSG:26917 instead. Error: {e}")
                header.add_crs(pyproj.CRS.from_epsg(26917)) 

        las = laspy.LasData(header)
        las.x = aligned_data["x"]
        las.y = aligned_data["y"]
        las.z = aligned_data["z"]
        las.classification = aligned_data["classification"]
        las.intensity = aligned_data["intensity"]
        las.return_number = aligned_data["return_number"]
        las.num_returns = aligned_data["num_returns"]

        las.write(self.aligned_path)
        print(f"Saved aligned LAS file to {self.aligned_path} with CRS applied.")


