import torch
from pytorch3d.ops import iterative_closest_point
import laspy
import numpy as np
import pyproj
import os

class Pytorch3DICP:
    def __init__(self, source_path, target_path, device="cuda"):
        """
        Class to perform ICP alignment using PyTorch3D (GPU Accelerated).

        :param source_path: Path to the source point cloud (.las or .laz)
        :param target_path: Path to the target point cloud (.las or .laz)
        :param device: "cuda" for GPU, "cpu" for CPU (default: GPU)
        """
        self.source_path = source_path
        self.target_path = target_path
        self.device = torch.device(device if torch.cuda.is_available() else "cpu")  # Auto-detect GPU
        self.aligned_path = self._generate_aligned_path(source_path)
        self.transformation = None
        self.rmse = None
        self.metadata = None  # Store original LAS metadata
        self.original_crs = None  # Store original CRS

    def align(self):
        """ Performs ICP alignment using PyTorch3D and saves the aligned point cloud. """
        source_points, source_metadata = self._load_point_cloud(self.source_path)
        target_points, _ = self._load_point_cloud(self.target_path)

        if source_points is None or target_points is None:
            print("Error: Failed to load point clouds.")
            return None

        # Convert to PyTorch tensors and move to GPU
        source_tensor = torch.tensor(source_points, dtype=torch.float32, device=self.device)
        target_tensor = torch.tensor(target_points, dtype=torch.float32, device=self.device)

        try:
            # Run GPU-accelerated ICP using Pytorch3D
            transformation_matrix, rmse = iterative_closest_point(
                source_tensor.unsqueeze(0), target_tensor.unsqueeze(0), max_iterations=50
            )

            self.transformation = transformation_matrix.squeeze().cpu().numpy()  # Convert back to NumPy
            self.rmse = rmse.item()

            # Apply transformation to source points
            source_homo = np.hstack((source_points, np.ones((source_points.shape[0], 1))))
            transformed_source = source_homo @ self.transformation.T
            transformed_source = transformed_source[:, :3]  # Drop homogeneous coordinate

            # Merge metadata and save LAS file
            aligned_data = self._merge_metadata(transformed_source, source_metadata)
            self._save_as_laz(aligned_data)

            # Return output
            output_str = f"""
            GPU-Accelerated ICP Completed.
            RMSE: {self.rmse:.6f}
            Transformation Matrix:
            {np.array_str(self.transformation, precision=6, suppress_small=True)}
            """
            return self.aligned_path, output_str.strip()

        except Exception as e:
            return None, f"Error during ICP alignment: {str(e)}"

    def _load_point_cloud(self, file_path):
        """ Loads a LAS file and returns points and metadata. """
        if file_path.endswith(".las") or file_path.endswith(".laz"):
            with laspy.open(file_path) as las_file:
                las = las_file.read()
                points = np.vstack((las.x, las.y, las.z)).astype(np.float32).transpose()

                # Store metadata
                metadata = {
                    "classification": las.classification.copy(),
                    "intensity": las.intensity.copy(),
                    "return_number": las.return_number.copy(),
                    "num_returns": las.num_returns.copy(),
                }

                # Store CRS
                try:
                    crs = las.header.parse_crs()
                    self.original_crs = crs if crs else pyproj.CRS.from_epsg(26917)
                except Exception:
                    self.original_crs = pyproj.CRS.from_epsg(26917)

                return points, metadata

        else:
            print("Unsupported file format")
            return None, None

    def _generate_aligned_path(self, source_path):
        """ Generates a filename for the aligned LAS file. """
        base, _ = os.path.splitext(source_path)
        return f"{base}_aligned_gpu.laz"

    def _merge_metadata(self, transformed_points, metadata):
        """ Merges transformed XYZ with original metadata. """
        num_points = len(transformed_points)
        min_len = min(num_points, len(metadata["classification"]))

        aligned_data = np.zeros(num_points, dtype=[
            ("x", "f4"), ("y", "f4"), ("z", "f4"),
            ("classification", "u1"), ("intensity", "u2"),
            ("return_number", "u1"), ("num_returns", "u1")
        ])

        aligned_data["x"] = transformed_points[:min_len, 0]
        aligned_data["y"] = transformed_points[:min_len, 1]
        aligned_data["z"] = transformed_points[:min_len, 2]
        aligned_data["classification"] = metadata["classification"][:min_len]
        aligned_data["intensity"] = metadata["intensity"][:min_len]
        aligned_data["return_number"] = metadata["return_number"][:min_len]
        aligned_data["num_returns"] = metadata["num_returns"][:min_len]

        return aligned_data

    def _save_as_laz(self, aligned_data):
        """ Saves transformed LAS file with original metadata. """
        header = laspy.LasHeader(point_format=3, version="1.4")
        header.offsets = np.min(aligned_data[["x", "y", "z"]], axis=0)
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

# # Example Usage
# if __name__ == "__main__":
#     source_path = "F:/Thesis/TTP/Data/LAS/Aligned/TTP15A.laz"
#     target_path = "F:/Thesis/TTP/Data/LAS/Aligned/TTP19A.laz"

#     icp_aligner = Pytorch3DICP(source_path, target_path)
#     aligned_file, output_str = icp_aligner.align()

#     if aligned_file:
#         print(f"Aligned file created at: {aligned_file}")
#         print(output_str)
#     else:
#         print("Alignment failed.")
