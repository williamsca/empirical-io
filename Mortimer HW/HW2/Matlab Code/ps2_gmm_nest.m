function [objFun, newWeightMat] = ps2_gmm_nest(params, X, Y, Z, share, weightMat)

beta = params(1:5);
sigma = params(6);

nMoms = size(Z,2);
nObs = size(Z,1);
epsilon = Y - X*beta - share .* sigma;
moms = Z'*epsilon/nObs;

objFun = moms'*weightMat*moms;

temp = Z.*repmat(epsilon,1,nMoms);
newWeightMat = inv(temp'*temp/nObs);
end