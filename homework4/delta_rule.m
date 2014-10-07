%Reading the files in and labeling them as appropriate
clear;
load('HW4/DeltaRuleClass1Training.txt');
load('HW4/DeltaRuleClass2Training.txt');
load('HW4/PDRClass1Testing.txt');
load('HW4/PDRClass2Testing.txt');

n = length(DeltaRuleClass1Training(:,1));
c1_train = horzcat(DeltaRuleClass1Training(:,2:3), ones(n,1));
c2_train = horzcat(DeltaRuleClass2Training(:,2:3), -ones(n,1));

n = length(PDRClass1Testing(:,1));
c1_testing = horzcat(PDRClass1Testing(:,2:3), ones(n,1));
c2_testing = horzcat(PDRClass2Testing(:,2:3), -ones(n,1));

training_set = [c1_train; c2_train];
testing_set = [c1_testing; c2_testing];

%stopping criteria
epsilon = 0.005;

%initial scatterplot of the data in x1,x2 coordinates
figure
hold on;
xlabel('dimension: x1');
ylabel('dimension: x2');
title('Weight Vector and Decision Boundaries (eta variation)')

scatter(c1_train(:,1), c1_train(:,2),'.');
scatter(c2_train(:,1), c2_train(:,2),'.');

eta_values = 0.0001 : 0.0001 : 0.001;

%Output: eta value, total epochs, training error, testing error
eta_variation_output = [0,0,0,0];
rand('seed', 12343);

for j = 1 : length(eta_values')
    eta = eta_values(j);
    loop = true;
    epoch = 0;
    p = [0,0];
    first = true;
    last_error = 0;
    
    while (loop)
        %sprintf('weights: %f %f', p(1), p(2))
        [training_error, testing_error, new_weights] = runEpoch_delta(p, training_set, testing_set, eta);
       % sprintf('new weights %f %f', new_weights(1), new_weights(2))
        p = new_weights;

        if (training_error - last_error < epsilon)
            loop = false;
        end
        
        last_error = training_error;

        epoch = epoch + 1;
        eta_variation_output = [eta_variation_output; [eta, epoch, training_error, testing_error]];
    end
    line([0,p(1)], [0, p(2)],'LineStyle', '--', 'color', 'r');
    minv = -1 / (new_weights(2)/new_weights(1));
    line([-2, 2], [-2*minv, 2*minv],'LineStyle', '--', 'color', 'k');
end

sprintf('done1')
eta_variation_output = eta_variation_output(2:(length(eta_variation_output(:,1))), :);
legend('class 1', 'class 2', 'black lines : decision boundaries', 'red lines: weight vectors', 'location', 'southeast');
axis([-6,6,-5,5]);

% analyzing distribution of initial weights
initial_weights = rand(5,2);
initial_weights = [initial_weights;
                   10 * initial_weights - 5;
                   100* initial_weights - 50;
                   1000* initial_weights - 500;
                   10000* initial_weights - 5000];

eta = 0.0001;
weights_variation_output = [0,0,0,0,0];

for j = 1 : length(initial_weights(:,1))
    p = initial_weights(j, :);
    epoch = 0;
    loop = true;
    first = true;
    last_error = 0;
    while (loop)
        [training_error, testing_error, new_weights] = runEpoch_delta(p, training_set, testing_set, eta);
        
        p = new_weights;
        
        if (training_error - last_error < epsilon)
            loop = false;
        end
        last_error = training_error;

        epoch = epoch + 1;
    end
    weights_variation_output = [weights_variation_output; [p(1), p(2), epoch, training_error, testing_error]];
end
sprintf('done with weight variation')

weights_variation_output = weights_variation_output(2:(length(weights_variation_output(:,1))), :);
figure;
hold on;
xlabel('Trial Number');
ylabel('Number of Epochs');
title('Number of Epochs Needed for Convergence');
scatter(1:5, weights_variation_output(1:5,3));
scatter(6:10, weights_variation_output(6:10,3));
scatter(11:15, weights_variation_output(11:15,3));
scatter(16:20, weights_variation_output(16:20,3));
scatter(21:25, weights_variation_output(21:25,3));
axis([0,25, 0, 10]);
legend('1-5: [0,1)', '6-10: [-5,5)', '11-15: [-50,50)', '16-20: [-500, 500)', '21-25: [-5000,5000)', 'location', 'NorthWest');


%presentation ordering analysis
num_perms = 20;
perm_variation_output = ones(1,num_perms);
eta = 0.0001;

for j = 1:num_perms
    training_presentations = training_set(randperm(length(training_set(:,1))),:);
    loop = true;
    
    p = 100*rand(1,2);
    epoch = 0;
    first = true;
    last_error = 0;
    while (loop)
        [training_error, testing_error, new_weights] = runEpoch_delta(p, training_presentations, testing_set, eta);
        
        p = new_weights;

        if (training_error - last_error < epsilon)
            loop = false;
        end
        last_error = training_error;
        
        epoch = epoch + 1;
    end
    perm_variation_output(j) = epoch;
end

figure;
hold on;
xlabel('permutation number');
ylabel('number of epochs');
title('Epochs until convergence with random permutation of presentations');
scatter(1:20, perm_variation_output);
p = polyfit(1:20, perm_variation_output, 1);
ys = polyval(p, 1:20);
line(1:20, ys);
axis([0,20,0,15]);
