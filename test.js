
function new_neural_network(num_input, num_hidden, num_output) {
  var i, h, o, data;
  data = {
    allowedError: 0.05,
    numInput: num_input,
    numHidden: num_hidden,
    numOutput: num_output,
    inputs: [],  // num_input
    hidden: [],  // num_hidden
    outputs: [], // num_output
    w1: [],      // num_input  * num_hidden
    w2: [],      // num_hidden * num_output
    output_errors: [], // num_output
    hidden_errors: [], // num_hidden),
    input_training_examples: [],
    output_training_examples: []
  };
  for (i = 0; i < num_input; i += 1) {
    data.w1[i] = []; // num_hidden
    data.inputs[i] = 0;
  }
  for (i = 0; i < num_hidden; i += 1) {
    data.w2[i] = []; // num_output);
  }
    console.log(data.w1);
    console.log(data.w2);
  for (i = 0; i < num_input; i += 1) {
    for (h = 0; h < num_hidden; h += 1) {
      data.w1[i][h] = 0.01 * (Math.random() - 0.005);
    }
  }
    console.log(data.w1);
}

new_neural_network(3, 3, 3);
