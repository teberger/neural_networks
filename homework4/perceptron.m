clear;
load('HW4/PercepClass1Training.txt');
load('HW4/PercepClass2Training.txt');
load('HW4/PDRClass1Testing.txt');
load('HW4/PDRClass2Testing.txt');

n = length(PercepClass1Training(:,1));
c1_train = horzcat(PercepClass1Training(:,2:3), ones(n,1));
c2_train = horzcat(PercepClass2Training(:,2:3), -ones(n,1));

n = length(PDRClass1Testing(:,1));
c1_testing = horzcat(PDRClass1Testing(:,2:3), ones(n,1));
c2_testing = horzcat(PDRClass2Testing(:,2:3), -ones(n,1));

training_set = [c1_train; c2_train];
testing_set = [c1_testing; c2_testing];

figure
hold on;
scatter(c1_testing(:,1), c1_testing(:,2), '.');
scatter(c2_testing(:,1), c2_testing(:,2), '.');
hold off;

figure
hold on;
scatter(c1_train(:,1), c1_train(:,2),'.');
scatter(c2_train(:,1), c2_train(:,2),'.');

eta_values = 0.001 : 0.01 : 0.10;

%Output: eta value, total epochs, training error, testing error
eta_variation_output = [0,0,0,0,0];
rand('seed', 12343);
for j = 1 : length(eta_values')
    eta = eta_values(j);
    loop = true;
    epoch = 0;
    p = [0,0];
    while (loop)
        [training_error, testing_error, p] = runEpoch(p, training_set, testing_set, eta);

        if training_error == 0
            loop = false;
        end

        epoch = epoch + 1;
        eta_variation_output = [eta_variation_output; [eta, epoch, training_error, testing_error, scale]];
    end
end
eta_variation_output = eta_variation_output(2:(length(eta_variation_output(:,1))), :);
%hold on;
%figure
%scatter(output(:,2), output(:,3), '.');
%scatter(output(:,2), output(:,4), '.');