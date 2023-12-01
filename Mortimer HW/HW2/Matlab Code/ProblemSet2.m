%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem Set 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear
clc

data = load('data_ps2_3nests.txt');

car = data(:,1);
year = data(:,2);
firm = data(:,3);
p = data(:,4);
q = data(:,5);
wt = data(:,6);
hp = data(:,7);
ac = data(:,8);
nest3 = data(:,9);
[J,~] = size(data);

M = 100000000;

years = year - 1989 .* ones(J,1);
groupSums = accumarray(years, q, [], @sum);
repeatedGroupSums = groupSums(years);   % Repeat the group sums for each row within the group
S_0t = ones(J,1) - M^(-1) .* repeatedGroupSums;
S_jt = q ./ M;


%% 1. Logit GMM
delta_jt = log(S_jt) - log(S_0t);

% create IVs (Z)
IVwt = zeros(131,1);
IVhp = zeros(131,1);
IVac = zeros(131,1);

for j=1:J
    id = firm(j,1);
    yearj = year(j,1);
    IVwt(j,1) = sum(wt(:,1).*(id~=firm(:,1)).*(yearj == year))./sum((id~=firm(:,1)).*(yearj == year)); 
    IVhp(j,1) = sum(hp(:,1).*(id~=firm(:,1)).*(yearj == year))./sum((id~=firm(:,1)).*(yearj == year)); 
    IVac(j,1) = sum(ac(:,1).*(id~=firm(:,1)).*(yearj == year))./sum((id~=firm(:,1)).*(yearj == year)); 
end

% normalize weight, horsepower, and price
mean_wt = mean(wt,1);
wt = wt ./ mean_wt;
mean_hp = mean(hp,1);
hp = hp ./ mean_hp;
mean_p = mean(p,1);
p = p ./ mean_p;

X = [ones(J,1), wt, hp, ac, p];
Z = [ones(J,1), wt, hp, ac, IVwt, IVhp, IVac];



param0 = ones(5,1);
weightMat = eye(7);

% GMM: logit
gmmOptions = optimset('MaxFunEval', 1e6, 'MaxIter', 1e15, 'TolFun', 1e-10);
param_logit = fminsearch( @(param) ps2_gmm(param, X, delta_jt, Z, weightMat),param0,gmmOptions);
[~, optWeightMat_logit] = ps2_gmm(param_logit, X, delta_jt, Z, weightMat);
param_logit_optW = fminsearch( @(param) ps2_gmm(param, X, delta_jt, Z, optWeightMat_logit) ...
    ,param_logit,gmmOptions);



%% Nested Logit

% Generate nest level sales
S_gt = zeros(J,1);
for j = 1:J
    g = nest3(j);
    y = year(j);
    group_q = sum(q(:,1) .* (nest3==g) .* (year==y));
    S_gt(j) = q(j) ./ group_q;
end
share = log(S_gt);

% create new IVs for nested logit (Z_nl) 
nIVwt = zeros(131,1);
nIVhp = zeros(131,1);
nIVac = zeros(131,1);
for j=1:J
    own = car(j);
    g = nest3(j,1);
    yearj = year(j,1);
    nIVwt(j,1) = sum(wt(:,1).*(own~=car(:,1)).*(yearj == year(:,1)).*(g == nest3(:,1))) ...
        ./sum((own~=car(:,1)).*(yearj == year(:,1)).*(g == nest3(:,1))); 
    nIVhp(j,1) = sum(hp(:,1).*(own~=car(:,1)).*(yearj == year(:,1)).*(g == nest3(:,1))) ...
        ./sum((own~=car(:,1)).*(yearj == year(:,1)).*(g == nest3(:,1))); 
    nIVac(j,1) = sum(ac(:,1).*(own~=car(:,1)).*(yearj == year(:,1)).*(g == nest3(:,1))) ...
        ./sum((own~=car(:,1)).*(yearj == year(:,1)).*(g == nest3(:,1)));  
end

Z_nl = [ones(J,1), wt, hp, ac, IVwt, IVhp, IVac, nIVwt, nIVhp, nIVac];


% 2sls
X_long = [X share];
param_2sls = (Z_nl'*X_long)\(Z_nl'*delta_jt);


% GMM: nested logit
%param00 = [param_logit_optW; 0.9];
param00=param_2sls;
weightMat = eye(10);
param_nestedlogit = fminsearch( @(param) ps2_gmm_nest(param, X, delta_jt, Z_nl, share, weightMat) ...
    ,param00, gmmOptions);
[~, optWeightMat_nestedlogit] = ps2_gmm_nest(param_nestedlogit, X, delta_jt, Z_nl, share, weightMat);
param_nestedlogit_optW = fminsearch( @(param) ps2_gmm_nest(param, X, delta_jt, Z_nl, share, optWeightMat_nestedlogit) ...
    ,param_nestedlogit,gmmOptions);



%% elasticity
% Identify the top 10 cars
data_app = [data S_jt S_gt];
data_sort = sortrows(data_app, 5,"descend");
data_sort_top10 = data_sort(1:10,:);
s = data_sort_top10(:,10);
s_g = data_sort_top10(:,11);
p_short = data_sort_top10(:,4) ./ mean_p;

logit_el_own = param_logit_optW(5) .* (ones(10,1) - s) .* s;
logit_el_cross = -param_logit_optW(5) .* s * s';
priceM = diag(p_short); 
shareM = diag(s.^(-1));
matrix_q4 = priceM * logit_el_cross * shareM ;  %generate s_ij = elas *p_i/s_j
for i=1:10  
    matrix_q4(i,i) = logit_el_own(i) .* p_short(i) ./ s(i);
end

nestedlogit_el_own = (ones(10,1) - (1-param_nestedlogit_optW(6)) .* s - param_nestedlogit_optW(6) .* s_g)...
    .* s .* param_nestedlogit_optW(5) ./(1-param_nestedlogit_optW(6));
nestedlogit_el_cross =transpose( -param_nestedlogit_optW(5) .* ...
    (s + param_nestedlogit_optW(6).*s_g./(1-param_nestedlogit_optW(6))) * s');
matrix_q7 = priceM * nestedlogit_el_cross * shareM;
for i=1:10
    matrix_q7(i,i) = nestedlogit_el_own(i) .* p_short(i) ./ s(i);
end
