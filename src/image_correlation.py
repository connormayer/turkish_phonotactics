from PIL import Image
import numpy as np

img = Image.open('data/tiger.jpg').convert('L')
img_array = np.asarray(img)

best = 0
best_thresh = None
best_array = None

for threshold in range(5, 255, 5):
	print(threshold)
	categorical_array = np.uint(img_array > threshold)
	v1 = np.ndarray.flatten(img_array)
	v2 = np.ndarray.flatten(categorical_array)
	corr = np.corrcoef(v1, v2)
	if corr[0, 1] > best:
		best = corr[0, 1]
		best_thresh = threshold
		best_array = categorical_array

best_image = Image.fromarray(np.uint8(best_array * 255))
best_image.save('figs/tiger_categorical.jpg')
print("Best threshold: {}".format(best_thresh))
print("Best correlation: {}".format(best))