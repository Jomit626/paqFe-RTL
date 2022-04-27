module SDPUltraRam # (
  parameter ADDR_WIDTH = 16,
  parameter DATA_WIDTH = 32
) (
  input wire clock,
  input wire reset,

  input wire wea,
  input wire [ADDR_WIDTH-1:0] addra,
  input wire [DATA_WIDTH-1:0] dina,

  input wire enb,
  input wire [ADDR_WIDTH-1:0] addrb,
  output reg [DATA_WIDTH-1:0] dob
);

  (* ram_style = "ultra" *)
  reg [DATA_WIDTH-1:0] mem[(1<<ADDR_WIDTH)-1:0];
  
  always @(posedge clock) begin
    if(wea) begin
      mem[addra] <= dina;
    end
  end

  always @(posedge clock) begin
    if(enb) begin
      dob <= mem[addrb];
    end
  end

endmodule