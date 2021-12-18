module ByteWriteTDPRamBL # (
  parameter NUM_COL = 4,
  parameter ADDR_WIDTH = 12,
  parameter DATA_WIDTH = NUM_COL * 8
) (

  input wire clk,
  input wire ena,
  input wire [NUM_COL-1:0] wea,
  input wire [ADDR_WIDTH-1:0] addra,
  input wire [DATA_WIDTH-1:0] dina,
  output wire [DATA_WIDTH-1:0] doa,

  input wire enb,
  input wire [ADDR_WIDTH-1:0] addrb,
  output wire [DATA_WIDTH-1:0] dob

);
  reg [DATA_WIDTH-1:0] ram [2**ADDR_WIDTH-1:0] ;

  reg [DATA_WIDTH-1:0] data_out_a;
  reg [DATA_WIDTH-1:0] data_out_b;

  assign doa = data_out_a;

  integer i;
  always @(posedge clk) begin
    if(ena) begin
      for(i=0;i<NUM_COL;i=i+1) begin
        if(wea[i]) begin
          ram[addra][i*8 +: 8] = dina[i*8 +: 8];
        end 
      end
      data_out_a <= ram[addra];
    end
  end

  assign dob = data_out_b;
  
  always @(posedge clk) begin
    if(enb) begin
        data_out_b <= ram[addrb];
    end
  end

endmodule