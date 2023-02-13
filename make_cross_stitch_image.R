## Step 1: Import any image you want to process
# The input of of this function is the file name of the image and a list of k values.
# The first element in the cluster_info is the data frame of our image. For each pixel(data point), there are 6 variables: 'x', 'y', 'R', 'G', 'B', 'col'. The first two indicates the position of pixel; 'R', 'G', 'B' and 'col' indicates the RGB color information for each of them.
# The second element in the cluster_info is the results of k-means clustering for each k on k-list.
cluster_info <- process_image('/Users/sunxiaotan/Desktop/University of Toronto/STA314/STA314 assignment1/314 A1/chapel.jpg', 2:20)

## Step 2: Draw scree plot
# The input of this function is the cluster_info list.
# The output contains two plots. On the left is the scree plot and on the right is the ratio plot. We can select the optimal k by either the smallest change in the left plot or the right plot.
scree_plot(cluster_info)

## Step 3: Show the full color strips
#The input of this function is the cluster_info list created by the first function.
#The output is color strips for all the DMC colors of clustering centers. Since we're using the most complex clustering, we can select the optimal k by viewing these colors and decide how many of them are distinct enough.
color_strips(cluster_info)

## Step 4: Make cross-stitch pattern
# Subjectively select optimal k to be 15. Select the total number of possible stitches in the horizontal direction is 500.
set.seed(1)
make_pattern(cluster_info, 15, 500)
