%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem Set 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear
clc

data = load('ps1_data_nohead.txt');
% sort the data by price
data_sort = sortrows(data);

price = data_sort(:,1);
quantity = data_sort(:,2);
weight = data_sort(:,3);
hp = data_sort(:,4);
ac = data_sort(:,5);
firm = data_sort(:,6);
[J,~] = size(data_sort);

M = 100000000;
lambda = 0.000004;
Svec = quantity ./M;

%% (1) Solve for delta

tot = sum(quantity);
delta = zeros(J,1);
delta(1) = - log(1-tot/M)*price(1)/lambda;
delta(2) = delta(1) - (price(2)-price(1))* log(Svec(1)+exp(-lambda*delta(1)/price(1)))/lambda;

for j = 3:J-1
    delta(j) = delta(j-1) ...
        -(price(j)-price(j-1))* log(Svec(j-1)+exp(-lambda*(delta(j-1)-delta(j-2))/(price(j-1)-price(j-2))))/lambda;
end

delta(J) = delta(J-1) -log(1- Svec(J))*(price(J)-price(J-1))/lambda;


% (2) Estimate beta
X = [ones(J,1), weight, hp, ac];
Y = delta;

% ols
nMoms = size(X,2);
beta2SLS = (X'*X)\(X'*Y);
weightMat = eye(nMoms);

gmmOptions = optimset('MaxFunEval', 1e6, 'MaxIter', 1e15, 'TolFun', 1e-10);
betaGmm1 = fminsearch( @(beta) ps1_gmm(beta, X, Y, weightMat), beta2SLS,gmmOptions);


%% (6) Supply side estimation

% identify ownership
lags = [1 -1];
LagFirm = lagmatrix(firm,lags);
f_minus1 = LagFirm(:, 1);
f_plus1 = LagFirm(:, 2);
sameFminus1 = (firm == f_minus1);
sameFplus1 = (firm == f_plus1);

% generate other lags
Lagdelta = lagmatrix(delta,lags);
Lagprice = lagmatrix(price,lags);
delta_minus1 = Lagdelta(:,1);
delta_plus1 = Lagdelta(:,2);
price_minus1 = Lagprice(:,1);
price_plus1 = Lagprice(:,2);


% create iv from rival characteristics
IVweight = zeros(J,1);
IVhp = zeros(J,1);
IVac = zeros(J,1);

for j=1:J
    idFirm = firm(j);
    temp_data = data_sort(firm ~= idFirm,:);
    IVweight(j) = sum(temp_data(:,3)); 
    IVhp(j) = sum(temp_data(:,4)); 
    IVac(j) = sum(temp_data(:,5)); 
end

Z = [ones(J,1), weight, hp, ac, IVweight, IVhp, IVac];


% own demand deriv
ownD = lambda*(exp(-lambda*(delta_plus1 - delta)./(price_plus1-price)) ./(price_plus1-price) ...
    +exp(-lambda*(delta - delta_minus1)./(price-price_minus1)) ./(price-price_minus1));
ownD(1) = lambda * delta(1)*exp(-lambda * delta(1)/price(1)) ./ (price(1)^2);
ownD(J) = -lambda*(delta(J)-delta(J-1))*exp(-lambda*(delta(J)-delta(J-1)) ...
    /(price(J)-price(J-1)))/(price(J)-price(J-1))^2;

crossD = zeros(J,J);
crossD(1,1) = lambda * delta(1)*exp(-lambda * delta(1)/price(1)) ./ (price(1)^2);
crossD(J, J) = ownD(J);
crossD(J,J-1) = - ownD(J);

for j=2:J-1

    crossD(j,j) = ownD(j);
    crossD(j,j-1) = sameFminus1(j)* (-lambda*(delta(j) - delta(j-1))* ...
        exp(-lambda*(delta(j) - delta(j-1))/(price(j)-price(j-1))) /((price(j)-price(j-1)))^2);
    crossD(j,j+1) = sameFplus1(j)* (lambda*(delta(j+1) - delta(j))* ...
        exp(-lambda*(delta(j+1) - delta(j))/(price(j+1)-price(j))) /((price(j+1)-price(j)))^2);
    
end

% Alernatively:
% crossD = zeros(J, J);
% 
% % Diagonal terms (j=j)
% crossD(2:J-1, 2:J-1) = diag(ownD(2:J-1));
% 
% % Off-diagonal terms (j=j-1)
% crossD(2:J-1, 1:J-2) = sameFminus1(2:J-1) .* (-lambda .* (delta(2:J-1) - delta(1:J-2)) ...
%     .* exp(-lambda .* (delta(2:J-1) - delta(1:J-2)) ./ (price(2:J-1) - price(1:J-2))) ...
%     ./ ((price(2:J-1) - price(1:J-2)) .^ 2));
% 
% % Off-diagonal terms (j=j+1)
% crossD(2:J-1, 3:J) = sameFplus1(2:J-1) .* (lambda .* (delta(3:J) - delta(2:J-1)) ...
%     .* exp(-lambda .* (delta(3:J) - delta(2:J-1)) ./ (price(3:J) - price(2:J-1))) ...
%     ./ ((price(3:J) - price(2:J-1)) .^ 2));
% 
% % Special cases (j=1 and j=J)
% crossD(1, 1) = lambda * delta(1) * exp(-lambda * delta(1) / price(1)) / (price(1) ^ 2);
% crossD(J, J) = ownD(J);
% crossD(J, J-1) = -ownD(J);



param0 = [beta2SLS;ones(7,1)];
weightMat = eye(11);

% marginal cost
param_GMM_mc = fminsearch( @(param) ps1_gmm_supply(param, X, price, quantity, Z, Y, 1, ownD, crossD, weightMat) ...
    ,param0,gmmOptions);
[~, optWeightMat_mc] = ps1_gmm_supply(param_GMM_mc, X, price, quantity, Z, Y, 1, ownD, crossD, weightMat);
param_GMM_mc_optW = fminsearch( @(param) ps1_gmm_supply(param, X, price, quantity, Z, Y, 1, ownD, crossD, optWeightMat_mc) ...
    ,param_GMM_mc,gmmOptions);

% single-product BN
param_GMM_sp = fminsearch( @(param) ps1_gmm_supply(param, X, price, quantity, Z, Y, 2, ownD, crossD, weightMat) ...
    ,param0,gmmOptions);
[~, optWeightMat_sp] = ps1_gmm_supply(param_GMM_sp, X, price, quantity, Z, Y, 2, ownD, crossD, weightMat);
param_GMM_sp_optW = fminsearch( @(param) ps1_gmm_supply(param, X, price, quantity, Z, Y, 2, ownD, crossD, optWeightMat_sp) ...
    ,param_GMM_sp,gmmOptions);

% multi-product BN
param_GMM_mp = fminsearch( @(param) ps1_gmm_supply(param, X, price, quantity, Z, Y, 3, ownD, crossD, weightMat) ...
    ,param0,gmmOptions);
[~, optWeightMat_mp] = ps1_gmm_supply(param_GMM_mp, X, price, quantity, Z, Y, 3, ownD, crossD, weightMat);
param_GMM_mp_optW = fminsearch( @(param) ps1_gmm_supply(param, X, price, quantity, Z, Y, 3, ownD, crossD, optWeightMat_mp) ...
    ,param_GMM_mp,gmmOptions);


% % test: symbolic var
% syms p1 p2 p3 d1 d2 d3
% f = symfun(exp(-lambda*(d3-d2)/(p3-p2)) - exp(-lambda*(d2-d1)/(p2-p1)),[p1 p2 p3 d1 d2 d3]);
% Df_own = diff(f,p2);
% Df_minus1 = diff(f,p2);
% DF_plus1 = diff(f, p3);
% 
% ownD = zeros(J,1);
% ownD(2:J-1) = subs(Df_own, {p1, p2, p3, d1, d2, d3}, ...
%     {price(1:J-2), price(2:J), price(3:J), delta(1:J-2), delta(2:J), delta(3:J)});
% 
% % test
% 
% syms p1 p2 p3 d1 d2 d3
% 
% % Define the symbolic function
% f = exp(-lambda*(d3-d2)/(p3-p2)) - exp(-lambda*(d2-d1)/(p2-p1));
% 
% % Calculate the derivatives
% Df_own = diff(f, p2);
% Df_minus1 = diff(f, p2);
% Df_plus1 = diff(f, p3);
% 
% % Initialize ownD
% ownD = zeros(J, 1);
% 
% % Calculate the values for ownD
% ownD(2:J-1) = subs(Df_own, {p1, p2, p3, d1, d2, d3}, ...
%     {price(1:J-2), price(2:J), price(3:J), delta(1:J-2), delta(2:J), delta(3:J)});

