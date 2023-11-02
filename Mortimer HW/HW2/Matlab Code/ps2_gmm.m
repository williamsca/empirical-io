function [objFun, newWeightMat] = ps2_gmm(params, X, Y, Z, weightMat)

nMoms = size(Z,2);
nObs = size(Z,1);
epsilon = Y - X*params;
moms = Z'*epsilon/nObs;

objFun = moms'*weightMat*moms;

temp = Z.*repmat(epsilon,1,nMoms);
newWeightMat = inv(temp'*temp/nObs);
end