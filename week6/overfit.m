
% Load data files
load in.dta;
load out.dta;

%
% Problem #2
% a) Ein ~ 0.028571, Eout ~ 0.08400
%

% First two columns are x features
X = in(:, 1:2);

% prepend x0 ones
X = [ones(size(X, 1), 1), X];

% Y vector
Y = in(:, 3);

% transform (1, x1, x2, x1^2, x2^2, x1*x2, |x1 - x2|, |x1 + x2|)
transform_X = [X, X(:, 2) .^ 2,            \
               X(:, 3) .^ 2,               \
               X(:, 2) .* X(:, 3),         \
               abs( X(:, 2) - X(:, 3) ),   \
               abs( X(:, 2) + X(:, 3) )];

% weights
w = pinv(transform_X' * transform_X) * transform_X' * Y;

% g(X)
g_x_in = transform_X * w;

% Ein
Ein = sum((g_x_in < 0) == (Y > 0)) / size(X, 1)

% First two columns are x features
test_X = out(:, 1:2);

% prepend x0 ones
test_X = [ones(size(test_X, 1), 1), test_X];

% Y vector
test_Y = out(:, 3);

% transform (1, x1, x2, x1^2, x2^2, x1*x2, |x1 - x2|, |x1 + x2|)
test_transform_X = [test_X, test_X(:, 2) .^ 2,            \
                    test_X(:, 3) .^ 2,                    \
                    test_X(:, 2) .* test_X(:, 3),         \
                    abs(test_X(:, 2) - test_X(:, 3)),     \
                    abs(test_X(:, 2) + test_X(:, 3))];

% g(X)
g_x_out = test_transform_X * w;

% Eout
Eout = sum((g_x_out < 0) == (test_Y > 0)) / size(test_X, 1)


%
% Problem #3
% 

lambda = 10^(-3);
decay_in = (lambda / size(X, 1)) * sum(w .^ 2);
% reg_Ein = (sum((g_x_in - Y) .^ 2) / size(X, 1)) + decay_in


w_reg_in = pinv((transform_X' * transform_X) + (lambda * eye(size(transform_X, 2)))) * transform_X' * Y;
reg_g_x_in = transform_X * w_reg_in;
reg_Ein = sum((reg_g_x_in < 0) == (Y > 0)) / size(X, 1) + decay_in

decay_out = (lambda / size(test_X, 1)) * sum(w .^ 2);

w_reg_out = pinv((test_transform_X' * test_transform_X) + (lambda * eye(size(test_transform_X, 2)))) * test_transform_X' * test_Y;
reg_g_x_out = test_transform_X * w_reg_out;
reg_Eout = sum((reg_g_x_out < 0) == (test_Y > 0)) / size(test_X, 1) + decay_out
