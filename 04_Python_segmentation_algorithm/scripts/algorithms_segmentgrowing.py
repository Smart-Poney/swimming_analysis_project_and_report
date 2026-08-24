import numpy as np
from typing import Tuple

def segment_error(
    x_curve: np.ndarray,
    y_curve: np.ndarray,
    left: int,
    right: int,
) -> float:
    
    """Calculates the error of a curve segment with respect to a line connecting its endpoints.
        This function approximates the area between a curve and a line segment by dividing the segment
        into trapezoids and summing their areas. It calculates the perpendicular distances
        from the curve's points to the line, and uses these distances as heights of the trapezoids.
        Args:
            x_curve (np.ndarray): X-coordinates of the curve.
            y_curve (np.ndarray): Y-coordinates of the curve.
            left (int): Left index of the segment.
            right (int): Right index of the segment.
        Returns:
            float: The calculated area between the curve segment and the line.
    """

    # Get only the values within the segment 
    x_curve = x_curve[left : right + 1]
    y_curve = y_curve[left : right + 1]
    

    # Caluculate the slope and intercept of the line that connects the first and last points of the curve
    denominator = x_curve[-1] - x_curve[0]
    slope = (y_curve[-1] - y_curve[0]) / denominator
    intercept = y_curve[0] - (slope * x_curve[0])

    # points on line where a perpendicular line form the straight line will intercept the curve 
    line_x = ( (slope * y_curve) - (slope*intercept) + x_curve ) / (slope**2 + 1)
    line_y = (slope * line_x) + intercept

    # Calculate the length of the line intercepting the curve and line at each datapoint
    heights = np.abs(np.sqrt((y_curve - line_y)**2 + (x_curve - line_x)**2))

    # Calculate the distances between datapoints on the line 
    distances = np.sqrt(np.diff(line_x)**2 + np.diff(line_y)**2)

    # Mask when curve is above the line
    above_line_mask = np.where(y_curve > line_y, True, False)

    # Mask when there is a change in the value of above_line_mask
    intercept_mask = np.diff(above_line_mask)

    area =  0.0
    for i in range(len(intercept_mask)):
        if intercept_mask[i] == True: 
            area += ((heights[i]**2 + heights[i+1]**2) / (2 * (heights[i] + heights[i+1]))) * distances[i]
        else:
            area += ((heights[i] + heights[i+1])/2) * distances[i]

    return area



def segment_growing(
    data: np.ndarray,
    xs: np.ndarray,
    ys: np.ndarray,
    num_rows: int,
    num_cols: int,
    thresh: float,
):
    
    """Performs segment growing to find optimal joint positions in a data array.
        This function iteratively refines joint positions based on error calculations
        within segments of the data. It aims to minimize error while placing joints
        effectively.
        
        Args:
            data (np.ndarray): The midline coordinates.
            xs (np.ndarray): Column numbers of x coordinates.
            ys (np.ndarray): Column numbers of y coordinates.
            num_rows (int): The number of rows in the data array.
            num_cols (int): The number of columns in the data array.
            thresh (float): The error threshold for segment acceptability.
        Returns:
            Tuple [np.ndarray, float, float]: A tuple containing:
                - joint_positions (np.ndarray): An array of optimal joint positions.
                - total_evaluations (float): The total number of error evaluations performed.
                - total_datapoints (float): The total number of datapoints the error metric processed.
    """

    joint_positions: np.ndarray = np.zeros(num_rows, dtype=int)
    free_index: int = 0
    total_evaluations: int = 0
    total_datapoints: int = 0
    i = 1
    j_prev = 0 if free_index == 0 else joint_positions[free_index - 1]


    # continue while there are unmeasured points and the previous joint is not at the end
    while i < (num_rows - j_prev) and j_prev < (num_rows-2):

        # For each frame get error from j_prev to j_prev + i
        frame_errors = np.zeros(xs.size)
        for frame in range(xs.size):
            x = data[:, xs[frame]]  
            y = data[:, ys[frame]]
            frame_errors[frame] = segment_error(x, y, j_prev, j_prev + i)
            total_datapoints += i
            total_evaluations += 1  # increase evaluations once


        # Place a joint at last valid position
        # if last valid position is the previous joint, place it one after
        # as resolution is too coarse for current threshold
        if np.max(frame_errors) > thresh:
            j_prev = j_prev + 1 if i == 1 else j_prev + (i-1)
            joint_positions[free_index] = j_prev
            free_index += 1  
            i = 1  
        else:
            i += 1  
    joint_positions = joint_positions[:free_index]
    return joint_positions, total_evaluations, total_datapoints




