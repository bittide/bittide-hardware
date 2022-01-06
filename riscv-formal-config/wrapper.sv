module rvfi_wrapper (
  input clock,
  input reset,
  `RVFI_OUTPUTS
);

(* keep *) `rvformal_rand_reg [31:0] iBusWishbone_DAT_MISO;
(* keep *) `rvformal_rand_reg iBusWishbone_ACK;
(* keep *) `rvformal_rand_reg [31:0] dBusWishbone_DAT_MISO;
(* keep *) `rvformal_rand_reg dBusWishbone_ACK;

(* keep *) wire [29:0] iBusWishbone_ADR;
(* keep *) wire [31:0] iBusWishbone_DAT_MOSI;
(* keep *) wire [3:0] iBusWishbone_SEL;
(* keep *) wire  iBusWishbone_CYC;
(* keep *) wire  iBusWishbone_STB;
(* keep *) wire  iBusWishbone_WE;
(* keep *) wire [2:0] iBusWishbone_CTI;
(* keep *) wire [1:0] iBusWishbone_BTE;
(* keep *) wire [29:0] dBusWishbone_ADR;
(* keep *) wire [31:0] dBusWishbone_DAT_MOSI;
(* keep *) wire [3:0] dBusWishbone_SEL;
(* keep *) wire  dBusWishbone_CYC;
(* keep *) wire  dBusWishbone_STB;
(* keep *) wire  dBusWishbone_WE;

contranomyRVFITE uut (
  .clk (clock),
  .reset (reset),

  .iBusWishbone_DAT_MISO (iBusWishbone_DAT_MISO),
  .iBusWishbone_ACK (iBusWishbone_ACK),
  .iBusWishbone_ERR (1'b0),
  .dBusWishbone_DAT_MISO (dBusWishbone_DAT_MISO),
  .dBusWishbone_ACK (dBusWishbone_ACK),
  .dBusWishbone_ERR (1'b0),
  .timerInterrupt (1'b0),
  .softwareInterrupt (1'b0),
  .externalInterrupt (32'd0),

  .iBusWishbone_ADR(iBusWishbone_ADR),
  .iBusWishbone_DAT_MOSI(iBusWishbone_DAT_MOSI),
  .iBusWishbone_SEL(iBusWishbone_SEL),
  .iBusWishbone_CYC(iBusWishbone_CYC),
  .iBusWishbone_STB(iBusWishbone_STB),
  .iBusWishbone_WE(iBusWishbone_WE),
  .iBusWishbone_CTI(iBusWishbone_CTI),
  .iBusWishbone_BTE(iBusWishbone_BTE),
  .dBusWishbone_ADR(dBusWishbone_ADR),
  .dBusWishbone_DAT_MOSI(dBusWishbone_DAT_MOSI),
  .dBusWishbone_SEL(dBusWishbone_SEL),
  .dBusWishbone_CYC(dBusWishbone_CYC),
  .dBusWishbone_STB(dBusWishbone_STB),
  .dBusWishbone_WE(dBusWishbone_WE),

  `RVFI_CONN
);

  // I-MEM
  always @(posedge clock) begin
    if (reset) begin
      assume (!iBusWishbone_ACK);
    end
    if (!iBusWishbone_CYC) begin
      assume (!iBusWishbone_ACK);
    end
  end

  // D-MEM
  always @(posedge clock) begin
    if (reset) begin
      assume (!dBusWishbone_ACK);
    end
    if (!dBusWishbone_CYC) begin
      assume (!dBusWishbone_ACK);
    end
  end

`ifdef CONTRANOMY_FAIRNESS
  reg [2:0] timeout_ibus = 0;
  reg [2:0] timeout_dbus = 0;

  always @(posedge clock) begin
    timeout_ibus <= 0;
    timeout_dbus <= 0;

    if (iBusWishbone_CYC && !iBusWishbone_ACK)
      timeout_ibus <= timeout_ibus + 1;

    if (dBusWishbone_CYC && !dBusWishbone_ACK)
      timeout_dbus <= timeout_dbus + 1;

    assume (!timeout_ibus[2]);
    assume (!timeout_dbus[2]);
  end
`endif

endmodule
