`define INPUT_FILE "input"
`define OUTPUT_FILE "output"

`include "../../Compressor.v"
`include "../../bytewrite_tdp_ram.v"

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
  output reg clk_160MHz,
  output reg clk_160MHz_rst,
  output reg clk_80MHz,
  output reg clk_80MHz_rst
);
  initial begin
    clk_160MHz = 0;  
    clk_80MHz = 0;

    clk_160MHz_rst = 0;
    clk_80MHz_rst = 0;

    #1000

    clk_160MHz_rst = 1;
    clk_80MHz_rst = 1;
  end

  always begin
    #3.125;
    clk_160MHz = ~clk_160MHz;
  end

  always begin
    #6.25;
    clk_80MHz = ~clk_80MHz;
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

module tb();
  wire clk_160MHz;
  wire clk_160MHz_rst_n;
  wire clk_80MHz;
  wire clk_80MHz_rst_n;

  clk_gen clk_gen_inst0(
    .clk_160MHz(clk_160MHz),
    .clk_160MHz_rst(clk_160MHz_rst_n),
    .clk_80MHz(clk_80MHz),
    .clk_80MHz_rst(clk_80MHz_rst_n)
  );

  wire model_clk = clk_160MHz;
  wire model_rst = ~clk_160MHz_rst_n;
  wire model_in_ready;
  wire model_in_valid;
  wire [7:0] model_in_bits_byte;
  wire model_in_bits_last;
  wire model_status_initDone;
  wire coder_clk = clk_80MHz;
  wire coder_rst = ~clk_80MHz_rst_n;
  wire coder_out_ready;
  wire coder_out_valid;
  wire [7:0] coder_out_bits_idx;
  wire [7:0] coder_out_bits_byte;
  wire coder_out_bits_last;

  Compressor dut(
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

  always @(posedge clk_160MHz) begin
    if(coder_out_ready && coder_out_valid && coder_out_bits_last) begin
      #1000 $finish();
    end
  end
endmodule

