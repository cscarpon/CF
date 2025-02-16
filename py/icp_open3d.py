import open3d as o3d
import numpy as np
import os
import laspy

class Open3DICP:
    def __init__(self, source_path, target_path, voxel_size=0.05, icp_method="point-to-point"):
        """
        Class to perform ICP alignment using Open3D and save results as .laz.

        :param source_path: Path to the source point cloud (.ply or .las)
        :param target_path: Path to the target point cloud (.ply or .las)
        :param voxel_size: Downsampling voxel size (smaller means higher resolution)
        :param icp_method: "point-to-point" or "point-to-plane"
        """
        self.source_path = source_path
        self.target_path = target_path
        self.aligned_path = self._generate_aligned_path(source_path)
        self.transformation = None
        self.rmse = None
        self.icp_method = icp_method

    def align(self):
        """ Performs ICP alignment and saves the aligned point cloud as .laz """
        # Load point clouds
        source = self._load_point_cloud(self.source_path)
        target = self._load_point_cloud(self.target_path)

        if source is None or target is None:
            print("Error loading point clouds.")
            return None

        # Downsample point clouds for efficiency
        source_down = source.voxel_down_sample(voxel_size=0.05)
        target_down = target.voxel_down_sample(voxel_size=0.05)

        # Estimate normals (needed for point-to-plane)
        if self.icp_method == "point-to-plane":
            source_down.estimate_normals(o3d.geometry.KDTreeSearchParamHybrid(radius=0.1, max_nn=30))
            target_down.estimate_normals(o3d.geometry.KDTreeSearchParamHybrid(radius=0.1, max_nn=30))

        # ICP Configuration
        threshold = 1.0  # Maximum correspondence distance
        trans_init = np.eye(4)  # Identity matrix as initial transformation

        if self.icp_method == "point-to-plane":
            icp_method = o3d.pipelines.registration.TransformationEstimationPointToPlane()
        else:
            icp_method = o3d.pipelines.registration.TransformationEstimationPointToPoint()

        # Run ICP
        try:
            result = o3d.pipelines.registration.registration_icp(
                source_down, target_down, threshold, trans_init, icp_method,
                o3d.pipelines.registration.ICPConvergenceCriteria(max_iteration=50)
            )

            self.transformation = result.transformation
            self.rmse = result.inlier_rmse

            print(f"ICP Alignment Completed.\nRMSE: {self.rmse}\nTransformation Matrix:\n{self.transformation}")

            # Apply transformation
            source.transform(self.transformation)

            # Convert Open3D point cloud to .laz and save
            self._save_as_laz(source)
            return self.aligned_path

        except Exception as e:
            print(f"Error during ICP alignment: {e}")
            return None

    def _load_point_cloud(self, file_path):
        """ Loads a point cloud from a .ply or .las file """
        if file_path.endswith(".ply"):
            return o3d.io.read_point_cloud(file_path)
        elif file_path.endswith(".las") or file_path.endswith(".laz"):
            print("Converting .las/.laz to .ply first...")
            
            with laspy.open(file_path) as las_file:
                las = las_file.read()
                points = np.vstack((las.x, las.y, las.z)).transpose()
                pcd = o3d.geometry.PointCloud()
                pcd.points = o3d.utility.Vector3dVector(points)

                temp_ply_path = file_path.replace(".las", ".ply").replace(".laz", ".ply")
                o3d.io.write_point_cloud(temp_ply_path, pcd)
                return o3d.io.read_point_cloud(temp_ply_path)

        else:
            print("Unsupported file format. Use .ply, .las, or .laz")
            return None

    def _generate_aligned_path(self, source_path):
        """ Generates a new file name for the aligned output as .laz """
        base, _ = os.path.splitext(source_path)
        return f"{base}_aligned.laz"

    def _save_as_laz(self, o3d_cloud):
        """ Saves an Open3D point cloud as a .laz file using laspy """
        points = np.asarray(o3d_cloud.points)

        # Create LAS header
        header = laspy.LasHeader(point_format=3, version="1.4")
        header.offsets = np.min(points, axis=0)
        header.scales = np.array([0.01, 0.01, 0.01])  # Adjust scale for precision

        # Create LAS file
        las = laspy.LasData(header)
        las.x = points[:, 0]
        las.y = points[:, 1]
        las.z = points[:, 2]

        # Save as .laz
        las.write(self.aligned_path)

# # Example Usage
# if __name__ == "__main__":
#     source_path = "F:/Thesis/TTP/Data/LAS/Aligned/TTP15A.laz"
#     target_path = "F:/Thesis/TTP/Data/LAS/Aligned/TTP19A.laz"

#     icp_aligner = Open3DICP(source_path, target_path, icp_method="point-to-plane")
#     aligned_file = icp_aligner.align()

#     if aligned_file:
#         print(f"Aligned file created at: {aligned_file}")
#     else:
#         print("Alignment failed.")
