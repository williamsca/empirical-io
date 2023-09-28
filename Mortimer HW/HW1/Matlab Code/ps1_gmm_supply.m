function [objFun, newWeightMat] = ps1_gmm_supply(params, X, p, q, Z, Y, eq_ass, ownD, crossD, weightMat)

beta = params(1:4);
gamma = params(5:8);
eta = params(9);

mom_indep = [X, Z];
nX = size(X,2);
nZ = size(Z,2);
nObs = size(X,1);
epsilon = Y - X*beta;

Svec = q ./ 100000000;

if eq_ass == 1 %mc, mc = p 
    omega = p-X*gamma - eta.*q;

else
    if eq_ass == 2  %single-product
        D = eye(nObs) * ownD;
    else %multi-product
        D = crossD;
    end

    omega = p-X*gamma - eta.*q + D\Svec;
end


moms = [X'*epsilon/nObs; Z'*omega/nObs];
objFun = moms'*weightMat*moms;

temp = mom_indep.*[repmat(epsilon,1,nX), repmat(omega,1,nZ)];
newWeightMat = inv(temp'*temp/nObs);

end