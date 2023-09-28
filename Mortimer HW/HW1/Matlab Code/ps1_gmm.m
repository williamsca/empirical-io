function [objFun] = ps1_gmm(params, X, Y, weightMat)

nMoms = size(X,2);
nObs = size(X,1);
epsilon = Y - X*params;
moms = X'*epsilon/nObs;

objFun = moms'*weightMat*moms;
end