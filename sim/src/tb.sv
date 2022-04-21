`include "../../genrtl/Compressor.v"

`define MODLE_PEIROD 12.500
`define MIXER_PEIROD 3.3333
`define CODER_PEIROD 25.000

module file2stream (
  input wire clk,
  input wire rst_n,
  output reg valid,
  output reg [7:0] data,
  output reg last,
  input wire ready
);
  
  integer fin;
  reg [7:0] data_next;
  initial begin
    valid = 0;
    data = 0;
    last = 0;
  end

  always @(posedge rst_n) begin
    if(rst_n) begin
      fin = $fopen(`INPUT_FILE, "rb");
      if(fin == -1) begin
        $error("failed to open output file.");
      end

      data = $fgetc(fin);
      data_next = $fgetc(fin);
      last = $feof(fin) == 0 ? 0 : 1;
      valid = 1;
    end
  end

  always @(posedge clk or negedge rst_n) begin
    if(!rst_n) begin
      valid = 0;
    end

    if(ready && valid) begin
      data = data_next;
      if(!last) begin
        data_next = $fgetc(fin);
        last = $feof(fin) == 0 ? 0 : 1;
      end else begin
        valid = 0;
      end
    end
  end
endmodule

module clk_gen (
  output reg model_clk,
  output reg model_rst_n,
  output reg coder_clk,
  output reg coder_rst_n,
  output reg mixer_clk,
  output reg mixer_rst_n
);
  initial begin
    model_clk = 0;  
    coder_clk = 0;
    mixer_clk = 0;

    model_rst_n = 0;
    coder_rst_n = 0;
    mixer_rst_n = 0;

    #1000

    model_rst_n = 1;
    coder_rst_n = 1;
    mixer_rst_n = 1;
  end

  always begin
    #(`MODLE_PEIROD / 2.0);
    model_clk <= ~model_clk;
  end

  always begin
    #(`CODER_PEIROD / 2.0);
    coder_clk <= ~coder_clk;
  end

  always begin
    #(`MIXER_PEIROD / 2.0);
    mixer_clk <= ~mixer_clk;
  end
endmodule

module stream2manyfile (
  input wire clk,
  input wire rst_n,
  input wire valid,
  input wire [7:0] data,
  input wire [7:0] idx,
  input wire last,
  output reg ready
);
  integer f, i;
  integer fout[7:0];

  string output_file = `OUTPUT_FILE;
  initial begin
    ready = 0;
  end

  always @(posedge rst_n) begin
    if(rst_n) begin

      output_file = {output_file,".0"};
      for(i=0;i<8;i=i+1) begin
        output_file.putc(output_file.len()-1,i + 48);

        f = $fopen(output_file, "wb");
        fout[i] = f;

        if(f == -1)
          $error("failed to open output file.");

        $display("output file: %s", output_file);
      end

      ready = 1;
    end
  end

  always @(posedge clk) begin
    if(!rst_n) begin
      ready = 0;
    end

    if(ready && valid) begin
      f = fout[idx[2:0]];
      $fwrite(f, "%c", data);
      if(last) begin
        ready <= 0;
      end
    end
  end
endmodule

module counter(
  input wire clk,
  input wire rst_n,
  input wire ce,
  output reg [31:0] d
);

  always @(posedge clk) begin
    if(~rst_n) 
      d <= 'd0;
    else if(ce)
      d <= d + 'd1;
  end

endmodule

module StreamMonitor (
  input wire clk,
  input wire rst_n,

  input wire valid,
  input wire ready,
  input wire last,

  output reg [31:0] transfer_time_counter,
  output reg [31:0] transfer_counter
);
  reg stateNxt;
  reg state;
  
  always @(posedge clk) begin
    if(~rst_n)
      state <= 'd0;
    else 
      state <= stateNxt;
  end

  always @(*) begin
    if(state == 'd0) begin
      if(valid & ready)
        stateNxt = 'd1;
      else
        stateNxt = 'd0;
    end else begin
      if(valid & ready && last)
        stateNxt = 'd0;
      else
        stateNxt = 'd1;
    end
  end

  always @(posedge clk) begin
    if(~rst_n)
      transfer_counter <= 'd0;
    else if (valid & ready)
      transfer_counter <= transfer_counter + 'd1;
  end

  always @(posedge clk) begin
    if(~rst_n)
      transfer_time_counter <= 'd0;
    else if(state == 'd1)
      transfer_time_counter <= transfer_time_counter + 'd1;
  end
endmodule

module tb();
  wire model_clk;
  wire model_rst_n;
  wire model_rst = ~model_rst_n;
  wire mixer_clk;
  wire mixer_rst_n;
  wire mixer_rst = ~mixer_rst_n;
  wire coder_clk;
  wire coder_rst_n;
  wire coder_rst = ~coder_rst_n;
  wire model_in_ready;
  wire model_in_valid;
  wire [7:0] model_in_bits_byte;
  wire model_in_bits_last;
  wire model_status_initDone;
  wire coder_out_ready;
  wire coder_out_valid;
  wire [7:0] coder_out_bits_idx;
  wire [7:0] coder_out_bits_byte;
  wire coder_out_bits_last;

  clk_gen clk_gen_inst0(
    .model_clk(model_clk),
    .model_rst_n(model_rst_n),
    .coder_clk(coder_clk),
    .coder_rst_n(coder_rst_n),
    .mixer_clk(mixer_clk),
    .mixer_rst_n(mixer_rst_n)
  );

  Compressor dut(
    .mixer_clk(mixer_clk),
    .mixer_rst(mixer_rst),
    .model_clk(model_clk),
    .model_rst(model_rst),
    .model_in_ready(model_in_ready),
    .model_in_valid(model_in_valid && model_status_initDone),
    .model_in_bits_byte(model_in_bits_byte),
    .model_in_bits_last(model_in_bits_last),
    .model_status_initDone(model_status_initDone),
    .coder_clk(coder_clk),
    .coder_rst(coder_rst),
    .coder_out_ready(coder_out_ready),
    .coder_out_valid(coder_out_valid),
    .coder_out_bits_idx(coder_out_bits_idx),
    .coder_out_bits_byte(coder_out_bits_byte),
    .coder_out_bits_last(coder_out_bits_last)
  );

  file2stream f2s(
    .clk(model_clk),
    .rst_n(~model_rst),
    .valid(model_in_valid),
    .ready(model_in_ready && model_status_initDone),
    .last(model_in_bits_last),
    .data(model_in_bits_byte)
    );
  

  stream2manyfile s2fs(
    .clk(coder_clk),
    .rst_n(~coder_rst),
    .valid(coder_out_valid),
    .ready(coder_out_ready),
    .idx(coder_out_bits_idx),
    .last(coder_out_bits_last),
    .data(coder_out_bits_byte)
  );

  wire [31:0] input_time_cnt;
  wire [31:0] input_cnt;
  StreamMonitor imon (
    .clk(model_clk),
    .rst_n(~model_rst),

    .valid(model_in_valid),
    .ready(model_in_ready && model_status_initDone),
    .last(model_in_bits_last),

    .transfer_time_counter(input_time_cnt),
    .transfer_counter(input_cnt)
  );

  wire [31:0] output_time_cnt;
  wire [31:0] output_cnt;
  StreamMonitor omon (
    .clk(coder_clk),
    .rst_n(~coder_rst),

    .valid(coder_out_valid),
    .ready(coder_out_ready),
    .last(coder_out_bits_last),

    .transfer_time_counter(output_time_cnt),
    .transfer_counter(output_cnt)
  );

  always @(posedge coder_clk) begin
    if(coder_out_ready && coder_out_valid && coder_out_bits_last) begin
      $display(
        "Input Stream Monitor\nClock Cnt: %d\nHandshake Cnt: %d\nTime : %dns\nTP:%d ns/Byte, %d MB/s"
        , input_time_cnt,
          input_cnt,
          input_time_cnt*(`MODLE_PEIROD),
          (input_time_cnt*(`MODLE_PEIROD)) / input_cnt,
          input_cnt/(input_time_cnt*(`MODLE_PEIROD)) * 953.67);
      $display(
        "Output Stream Monitor\nClock Cnt: %d\nHandshake Cnt: %d\n"
        , output_time_cnt, output_cnt);
      #1000 $finish();
    end
  end
endmodule

