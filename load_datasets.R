# ------------------------------------------------------------- #
# Loads and stores the datasets as variables.                   #
# ------------------------------------------------------------- #

if (sys.nframe() == 0){
  path = './datasets'
}


files <- c(
           'train_norm', 'train_pca_norm', 
           'test_norm', 'test_pca_norm', 
           
           'y_train', 
           'X_train_norm', 'X_train_stand', 'X_train_robstand', 
           'X_train_corr', 'X_train_robcorr', 'X_train_pbcorr',
           'X_train_pca_norm', 'X_train_pca_stand', 'X_train_robpca_robstand', 
           'X_train_pca_corr', 'X_train_robpca_robcorr',
           
           'y_test', 
           'X_test_norm', 'X_test_stand', 'X_test_robstand', 
           'X_test_corr', 'X_test_robcorr', 'X_test_pbcorr',
           'X_test_pca_norm', 'X_test_pca_stand', 'X_test_robpca_robstand', 
           'X_test_pca_corr', 'X_test_robpca_robcorr'
           
           )


for (name in files) {
  assign(name, read.csv(file.path(path, paste(name,'.csv', sep=''))))
}