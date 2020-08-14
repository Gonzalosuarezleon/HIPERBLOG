CREATE DEFINER=`root`@`localhost` PROCEDURE `MySQLReportesCartera`(NombreReporte Varchar(100), FechaInicio DATE, FechaFinal DATE, Usuario Char(10), VAsoEstado Char(1), Texto Varchar(40))
BEGIN
	
	DECLARE FechaIncioCorte, FECHAANTERIOR, FECHA_INICIO, FECHA_ANTERIOR, FECHACORTE Date;
	DECLARE ANO, MES, DIA, INICIO, FIN ,PERIODOANTAPO,PERIODOANTVIS,PERIODOANTPRO INT;
	DECLARE PERIODO CHAR(2);
	DECLARE VIGENCIA CHAR(4);
	DECLARE NNIT, CODIGO_SUSCRIPTOR VARCHAR(100);
	DECLARE TASAMORA DECIMAL(8,4);
	DECLARE vFormato, vCuentaPUC, vConcepto, vTraInf VARCHAR(100);
    DECLARE FECHA_INICIO_TXT VARCHAR(10);
    
    SET FECHA_INICIO_TXT = (SELECT EstParVal FROM estatutos WHERE EstParCod = 'FINICAS');
	SET FECHA_INICIO = CONCAT(SUBSTRING(FECHA_INICIO_TXT, 7, 4), '-', SUBSTRING(FECHA_INICIO_TXT, 4, 2), '-', SUBSTRING(FECHA_INICIO_TXT, 1, 2));
	SET FECHACORTE = FechaFinal;
    SET FECHA_ANTERIOR = LAST_DAY(DATE_ADD(FECHACORTE, INTERVAL -1 MONTH));
	SET CODIGO_SUSCRIPTOR = (SELECT EstParVal FROM estatutos WHERE EstParCod = 'DATACRE');

	DELETE FROM atlantiscoomuldenorteniif.TemporalArchivos WHERE TAUsuCon = Usuario;

	If NombreReporte = 'CARFECCORT' THEN	        -- REPORTE DE CARTERA A UNA FECHA DE CORTE --
				INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
											  `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
											  `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
											  `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
											  `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
											  `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
											  `Col49`, `Col50`,`TAUsuCon`)
				SELECT 	@ID := @ID + 1 ID,
						`Identificacion_N`, `Credito_T`, `FechaDesembolso_T`, `Descripcion_T`, `Diasmora_N`, 
						`Monto_N`, `SaldoCapital_N`, `SaldoInteres_N`, `SaldoMora_N`, `SaldoIntMora_N`, `Sucursal_T`,
						`ComprobanteDesembolso_T`, `Reporte_T`,`Altuta_N`, `Saldocuota_N`, `FechaProPag_T`, '(NULL)17', '(NULL)18',
						'(NULL)19', '(NULL)20', '(NULL)21', '(NULL)22', '(NULL)23', '(NULL)24', '(NULL)25', '(NULL)26',
						'(NULL)27', '(NULL)28', '(NULL)29', '(NULL)30', '(NULL)31', '(NULL)32', '(NULL)33', '(NULL)34',
						'(NULL)35', '(NULL)36', '(NULL)37', '(NULL)38', '(NULL)39', '(NULL)40', '(NULL)41', '(NULL)42', 
						'(NULL)43', '(NULL)44', '(NULL)45', '(NULL)46', '(NULL)47', '(NULL)48', '(NULL)49', '(NULL)50', -- 'ADMIN'
						 Usuario
				FROM (
						
						SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
									`Identificacion_N`, `Credito_T`, `FechaDesembolso_T`, `Descripcion_T`, `Diasmora_N`, 
									`Monto_N`, `SaldoCapital_N`, `SaldoInteres_N`, `SaldoMora_N`, `SaldoIntMora_N`, `Sucursal_T`,
									`ComprobanteDesembolso_T`, `Reporte_T`,`Altuta_N`, `Saldocuota_N`, `FechaProPag_T`	
						FROM (
								(							
								SELECT 	'No_N' `No_N`,'Identificacion_N' `Identificacion_N`,'Credito_T' `Credito_T`,'FechaDesembolso_T' `FechaDesembolso_T`,
								'Descripcion_T' `Descripcion_T`,'Diasmora_N' `Diasmora_N`,'Monto_N' `Monto_N`,'SaldoCapital_N' `SaldoCapital_N`,
								'SaldoInteres_N' `SaldoInteres_N`,'SaldoMora_N' `SaldoMora_N`,'SaldoIntMora_N' `SaldoIntMora_N`,'Sucursal_T' `Sucursal_T`,
								'ComprobanteDesembolso_T' `ComprobanteDesembolso_T`,'Reporte_T' `Reporte_T`,'Altuta_N' `Altuta_N`,'Saldocuota_N' `Saldocuota_N`,
								'FechaProPag_T' `FechaProPag_T`
								)
								UNION ALL
								(
								/*CRÉDITOS DESEMBOLSADOS antes de la fecha de corte*/
								SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)) `No_N`,
								Asoide `Identificacion_N`,carcod `Credito_T`,fecdescre `FechaDesembolso_T` ,' ' `Descripcion_T`,diamor `Diasmora_N`,Salinicre `Monto_N`
										,SalactCre `SaldoCapital_N`, salactint `SaldoInteres_N`, 0 `SaldoMora_N`, 0 `SaldoIntMora_N`, SucNom `Sucursal_T`,
										concat(carcmtipcom,trim(carcmprefijo),carcmnumcom) `ComprobanteDesembolso_T`,  'REPDIARIOCARTERA' `Altuta_N`,  '0' `Reporte_T`,
										'0' `Saldocuota_N`, '' `FechaProPag_T`
								from cartera  inner join asociados on asocod=carasocod
								inner join sucursales on sucpre=carcmprefijo
								Where Fecdescre<= FechaFinal and estcre<>'P' and  COMPROBANTEDESEMBOLSO <> 'X'
								)
							) T1
					) T1;

			-- actualizar el saldo de capital a fecha de corte
			UPDATE TemporalArchivos t INNER JOIN 
			(
				Select C1.carcod,c1.asalcre saldo,altura,saldocuota
				from 
				(
				select carcod,aFecReaPag,asalcre , apagcarcod,aNumCuoPag Altura,aSalpag SaldoCuota from pagoscarteraa where aFecReaPag <= FechaFinal
				) c1
				inner join
				(
				SELECT CARCOD, max(ApagCArCod) apagcarcod from pagoscarteraa where afecreapag <= FechaFinal GROUP BY CARCOD
				) c2 on c1.carcod=c2.carcod and c1.apagcarcod=c2.apagcarcod
			 ) t2 on t.col2=t2.carcod
			set col7=T2.saldo , col14=t2.Altura, col15=t2.SaldoCuota
			WHERE COL13='REPDIARIOCARTERA' and tausucon= Usuario;

			-- actualizar el saldo de creitos que no tienen registro en pagos cartera con la suma del capital e interes de la tabla de amortizacion
			update TemporalArchivos t inner join
			(
				select c1.carcod,sum(tabamovca) capital,sum(TabAmoVIn) interes,altcre
				from 
					(
						select carcod,TabAmoNCu,TabAmoVCa,TabAmoVIn from tabamocre
					) c1  inner join
					(
						select col2,col16,altcre from TemporalArchivos 
						inner join cartera on col2=carcod
						where col2 not in(select carcod from pagoscarteraa)
					)c2 on c1.carcod=c2.col2 
				   where TabAmoNCu>=c2.altcre group by c1.carcod
				) t2
			   on t.col2=t2.carcod
			 set Col7=capital ,col8=interes;
			-- actualiza fecha de proximo pago para creditos que no tengan registros en pagoscarteraa
			update TemporalArchivos t inner join
			(
			select c1.carcod,TabamoFpa fechaproxpago ,  altura,TabAmoNCu -- if(altura<=1,1,altura-1)
			from 
				(
					select carcod,TabamoFpa,TabAmoVCa,TabAmoVIn,TabAmoNCu from tabamocre
				) c1  inner join
				(
					select col2,col16,If(altcre<=1, 1,altcre) altura from TemporalArchivos 
					inner join cartera on col2=carcod
					where col2 not in(select carcod from pagoscarteraa)
				)c2 on c1.carcod=c2.col2 
			   where TabAmoNCu=altura ) t2 
			on t.col2=t2.carcod
			set Col16=fechaproxpago  where COL13='REPDIARIOCARTERA' and tausucon= Usuario;
			-- Eliminar creditos que el saldo a fecha de corte sea cero (0) en pagos cartera
			delete from atlantiscoomuldenorteniif.TemporalArchivos where TAUsuCon=usuario and Col7=0 and COL13='REPDIARIOCARTERA' and tausucon= Usuario;
			-- actualizar saldo y fecha de proximo pago de los creditos que estaban activos y no tenian registros en pagoscarteraa

			-- si la cuota no tiene saldo se adiciona una cuota paara buscar la fecha del proximo pago
			 Update TemporalArchivos set col14=Col14+1  where col15=0 and COL13='REPDIARIOCARTERA' and tausucon= Usuario;
			-- Actualizar la fecha del proximo pago a fecha decorte.
			update TemporalArchivos inner join tabamocre on col2=carcod and col14=TabamoNCu and TabAmoNPK=0
			  set col16=TabamoFPa where COL13='REPDIARIOCARTERA' and tausucon= Usuario;
			  
			-- actualizar el numero de comprobante de desembolso para creditos migrados
			update TemporalArchivos t inner join
			 (
				select carcod,Comprobantedesembolso comprobante from cartera where carcmcencos='' and Comprobantedesembolso is not null
			 )t2  on  t.col2=t2.carcod
			set t.Col12=t2.comprobante
			WHERE COL13='REPDIARIOCARTERA' and tausucon= Usuario;

			-- actualizar dias de mora
			update TemporalArchivos set col16 =  date_add(FechaFinal,interval +12 MONTH) where length(col16) < 10 and COL13='REPDIARIOCARTERA' and tausucon= Usuario;

			update TemporalArchivos t inner join
			(select col2 credito,col16,CalculoDComercial(convert(concat(substring(col16, 1, 4), '-',substring(col16, 6, 2), '-', substring(col16, 9, 2)), date),FechaFinal,'CAL') DiasMora
			 from TemporalArchivos where convert(concat(substring(col16, 1, 4), '-',substring(col16, 6, 2), '-', substring(col16, 9, 2)), date) < FechaFinal 
			and tasec>1 and (col16 is not null or col16<>'') and COL13='REPDIARIOCARTERA') t2 --
			on t.col2 = t2.credito 
			 set col5= DiasMora  where COL13='REPDIARIOCARTERA' and tausucon= Usuario;

			-- Actualizar saldo intereses del credito de los creditos que tienen registros en pagoscarteraa
			UPDATE TemporalArchivos t INNER JOIN 
			(	
			  select c3.carcod,sum(TabAmoVIn)  + saldointeres TabSaldoInteres,c4.Altura
			from
				(
					select carcod,TabAmoNCu,TabAmoVIn from tabamocre where TabAmoNPK=0
				)c3 inner join 
				(
					Select C1.carcod,c1.altura altura, c1.saldo saldo,c1.aSalInt saldointeres
					from 
					(
						select carcod,aNumCuoPag altura , apagcarcod,aSalCre saldo,aSalInt from pagoscarteraa where aFecReaPag<=FechaFinal
					) c1
					inner join
					(
						SELECT CARCOD, max(ApagCArCod) apagcarcod from pagoscarteraa where afecreapag<=FechaFinal and anumcuopk=0 GROUP BY CARCOD
					) c2 on c1.carcod=c2.carcod and c1.apagcarcod=c2.apagcarcod where saldo>0
				) c4 on c3.carcod=c4.carcod where c3.TabAmoNCu > c4.Altura
			   Group by c3.carcod 
			) t2 on t.col2=t2.carcod 
				set col8=T2.TabSaldoInteres 
			WHERE COL13='REPDIARIOCARTERA' and tausucon= Usuario;
			-- Actualizar saldo intereses del credito de los creditos que no tienen registros en pagoscarteraa
			update TemporalArchivos t inner join
			(
				Select c.carcod, sum(tabamovin) SaldoIntTabAmo from tabamocre t inner join cartera c on c.carcod=t.carcod  group by carcod
			)t2 on t.col2=t2.carcod
			set col8=SaldoIntTabAmo 
			where col2 not in(select carcod from pagoscarteraa where afecreapag <=FechaFinal) and col8=0 and  COL13='REPDIARIOCARTERA' and tausucon= Usuario;

			UPDATE TemporalArchivos SET Col4 = 'CREDITO EN MORA' where col5>0 and  COL13='REPDIARIOCARTERA' and tausucon= Usuario;
			UPDATE TemporalArchivos SET Col4 = 'CREDITO  AL DIA' where col5=0 and  COL13='REPDIARIOCARTERA' and tausucon= Usuario;

			-- calcular los valores de capital e interes en mora en una tabla temporal 
			CALL `MySQLTabAmoCARTERAFECCORFECCORTE`(Usuario,FechaFinal);

			call MySQLTabAmoActualizarSaldos (Usuario,FechaFinal);

END IF;

IF NombreReporte = 'CRESINCOM' THEN  -- REPORTE DE CRÉDITOS SIN COMPROBANTE
		  INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
				 `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
				 `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
				 `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
				 `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
				 `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
				 `Col49`, `Col50`,`TAUsuCon`)

		  SELECT @ID := @ID + 1, 
			`PAGADURIA_T`, `IDENTIFICACION_T`, `NOMBREASOCIADO_T`, `CODIGO_CREDITO_T`, `MONTO_N`, `SALDO_ACTUAL_N`, 
			`CAPITAL_APLICADO_N`, '(NULL)8', '(NULL)9', '(NULL)10', '(NULL)11', '(NULL)12', '(NULL)13', 
			'(NULL)14', '(NULL)15', '(NULL)16', '(NULL)17', '(NULL)18', '(NULL)19', '(NULL)20', '(NULL)21',
			'(NULL)22', '(NULL)23', '(NULL)24', '(NULL)25', '(NULL)26', '(NULL)27', '(NULL)28', '(NULL)29',
			'(NULL)30', '(NULL)31', '(NULL)32', '(NULL)33', '(NULL)34', '(NULL)35', '(NULL)36', '(NULL)37',
			'(NULL)38', '(NULL)39', '(NULL)40', '(NULL)41', '(NULL)42', '(NULL)43', '(NULL)44', '(NULL)45',
			'(NULL)46', '(NULL)47', '(NULL)48', '(NULL)49', '(NULL)50', Usuario
		  FROM (
			SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
			  `PAGADURIA_T`, `IDENTIFICACION_T`, `NOMBREASOCIADO_T`, `CODIGO_CREDITO_T`, `MONTO_N`, `SALDO_ACTUAL_N`, 
			  `CAPITAL_APLICADO_N`
			FROM (
			  SELECT 'PAGADURIA_T' `PAGADURIA_T`, 'IDENTIFICACION_T' `IDENTIFICACION_T`, 'NOMBREASOCIADO_T' `NOMBREASOCIADO_T`,
			  'CODIGO_CREDITO_T' `CODIGO_CREDITO_T`, 'MONTO_N' `MONTO_N`,'SALDO_ACTUAL_N' `SALDO_ACTUAL_N`, 
			  CAST('CAPITAL_APLICADO_N'AS CHARACTER) `CAPITAL_APLICADO_N` 
			  UNION ALL 
			  SELECT CARPAGEMPDCTNOM `PAGADURIA_T`, ASOIDE `IDENTIFICACION_T`, TRIM(CONCAT(IF(ASOPRINOM IS NULL, '', ASOPRINOM), ' ', IF(ASOSEGNOM IS NULL, '',ASOSEGNOM), ' ', IF(ASOPRIAPE IS NULL, ' ', ASOPRIAPE), ' ', IF(ASOSEGAPE IS NULL, ' ', ASOSEGAPE))) `NOMBREASOCIADO_T`,
				T1.CARCOD `CODIGO_CREDITO_T`, T1.SALINICRE `MONTO_N`, T1.SALACTCRE `SALDO_ACTUAL_N`, 
				T3.aValDisCap `CAPITAL_APLICADO_N`
			  FROM CARTERA T1 INNER JOIN ASOCIADOS T2 ON T1.CARASOCOD = T2.ASOCOD
				  INNER JOIN PAGOSCARTERAA T3 ON T1.CARCOD = T3.CARCOD
			  WHERE T3.AFECREAPAG >= FechaInicio AND T3.AFECREAPAG <= FechaFinal AND 
					(T3.APAGCARTIPCOM = '' or  T3.APAGCARTIPCOM IS NULL ) AND (T3.APAGCARNUMCOM = 0 or T3.APAGCARNUMCOM IS NULL)

			  ) T1
		   ) T1;

 END IF;

IF NombreReporte = 'RMOVDIA' or NombreReporte = 'RMOVCOMD' THEN
     drop table if exists PagAtl123;
     drop table if exists PagFin123;
     drop table if exists PagFin245; 

     -- /////// TEMPORAL PARA ALMACENAR LOS PAGOS DESDE ATLANTIS DEL PERIODO INGRESADO
     CREATE TEMPORARY TABLE PagAtl123 AS
    (
     --   Se seleccionan los registros de pago (Tipo C - M - P) para el periodo escogido 
     Select Concat(aPagCarTipCom, rtrim(aPagCarPrefijo), aPagcarNumCom)Comprobante, aFecReaPag Fecha_Pago, 0 Debito, Sum(aValDisCap)  credito, 1 Consulta
     from pagoscarteraa P
     Where aPagCarTipPag not in ('', 'N') and aFecReaPag Between FechaInicio and FechaFinal and aPagcarNumCom > 0 --  and PagcarNumCom = '95904'
     group by aFecReaPag, aPagCarTipCom, aPagCarPrefijo, aPagcarNumCom
    );
     
     --   Se seleccionan los registros de pago que fueron anulados para el periodo escogido 
     Insert  PagAtl123(Comprobante, Fecha_Pago, Debito, credito, Consulta)
     Select Concat(aPagCarTipComAnu,rtrim(aPagCarPrefijoAnu),aPagCarNumComAnu)Comprobante,aFecReaPagAnu, 0  ValorReal,   Sum(aValCapPagAnu) credito, 2
     from pagoscarteraanua
     Where aFecReaPagAnu Between FechaInicio and FechaFinal and aPagCarNumComAnu > 0 
     group by aFecReaPagAnu, aPagCarTipComAnu,aPagCarPrefijoAnu,aPagCarNumComAnu;
     
     --   Se seleccionan los registros de pago que fueron anulados para el periodo escogido 
     Insert  PagAtl123(Comprobante, Fecha_Pago, Debito, credito, Consulta)
     Select Concat(aTipComAnu,rtrim(aPrefijoAnu),aNumComAnu)Comprobante,aFecReaPagAnu, Sum(aValCapPagAnu)  ValorReal, 0 credito, 3
     from pagoscarteraanua
     Where aFecReaPagAnu Between FechaInicio and FechaFinal and aNumComAnu > 0 
     group by aFecReaPagAnu, aTipComAnu,aPrefijoAnu,aNumComAnu;
       
     Insert  PagAtl123(Comprobante, Fecha_Pago, Debito, credito, Consulta)
     Select Concat(aPagCarTipCom, rtrim(aPagCarPrefijo), aPagcarNumCom)Comprobante, LAST_DAY(convert(concat(substring(CarNomPer, 1, 4), substring(CarNomPer, 5, 2), DAY(FechaFinal)), date)), 0 ValorReal, Sum(aValDisCap) credito,6
     from pagoscarteraa P
   Inner Join carteranomina c
    on p.carcod = c.CarCod
   Inner Join carteranominapagos n
    on c.CarNomCod = n.CarNomCod
     and p.aPagCarCod = n.aPagCarCod
     Where aPagCarTipPag in ('N')  and aPagcarNumCom > 0 and 
    ((CarNomPer = Texto and CarNomPer = Year(FechaFinal ) * 100 + Month(FechaFinal )) or
    ((CarNomPer = Texto  and CarNomPer <> Year(FechaFinal ) * 100 + Month(FechaFinal) and aFecReaPag < FechaFinal )))
     group by aPagcarNumCom;                    
          -- -----------------------------------------------------------------------------------------------------------------------------------------------------------
     -- ----------------------------------------------------------------------DATOS DE DESEEMBOLSOS----------------------------------------------------------------
     insert  PagAtl123(Comprobante, Fecha_Pago, Debito, credito, Consulta)
     Select Concat(CarCMTipCom, trim(CarCMPrefijo), CarCMNumCom) Comprobante,  FecDesCre Fecha_Pago, SalIniCre Debito, 0 Credito, 4
     from Cartera  C
     Where FecDesCre Between FechaInicio  and FechaFinal and CarCMNumCom > 0 ;
   
     insert  PagAtl123(Comprobante, Fecha_Pago, Debito, credito, Consulta)
     Select Concat( CarAnuTipCom, trim(CarAnuPrefijo), CarAnuNumCom) Comprobante,  FecDesCre Fecha_Pago, 0 Debito, SalIniCre Credito, 5
     From Cartera 
     Where FecDesCre Between FechaInicio  and FechaFinal and CarAnuNumCom > 0;

    -- ----------------------------------------------------------------------DATOS DE DESEEMBOLSOS----------------------------------------------------------------
     -- -----------------------------------------------------------------------------------------------------------------------------------------------------------            
     -- /////// TEMPORAL PARA ALMACENAR LOS PAGOS DESDE FINANCOOP DEL PERIODO INGRESADO
     CREATE TEMPORARY TABLE PagFin123 AS
    (
     Select TransaccionDocCon Comprobante, transaccionfechadoc Fecha_Pago,  Sum(TransaccionDB)Debito, Sum(TransaccionCR)Credito, 0 Debito_Atlantis, 0 Credito_Atlantis
     from mantiscoomuldenorteniif.transacciones A
      Inner Join   mantiscoomuldenorteniif.transacciones1 B
    on A.transaccionId = B.TransaccionId
     where transaccionfechadoc Between FechaInicio and FechaFinal 
    and (CuentaID like'1469%' or CuentaID like'1411%' or CuentaID like'1412%' or  CuentaID like'1441%' or CuentaID like'1442%' or CuentaID like'1404%' or CuentaID like'1405%') and Not(TransaccionDocCon like 'W%')
      and Not (TransaccionDetalle Like '%Reclasificación de Crédito%' or TransaccionDetalle like '%Reversion:%')
     group by transaccionfechadoc, TransaccionDocCon
     -- order by 1, 2
    );
     --  Se actualiza los datos de los valores tomados desde Atlantis
     Update PagFin123 B
    Inner Join (Select comprobante, Fecha_pago, Sum(debito) debito, Sum(credito) credito
      from PagAtl123
      Group by comprobante, Fecha_pago) A    
    on A.Comprobante = B.Comprobante
     and A.Fecha_Pago = B.Fecha_Pago
    Set B.Debito_Atlantis = A.Debito, B.Credito_Atlantis = A.Credito;   
     -- /////// TEMPORAL PARA ALMACENAR LOS REGISTROS QUE ESTAN EN ATLANTIS QUE NO ESTAN EN FINANCOOP
     CREATE TEMPORARY TABLE PagFin245 AS
    ( 
     Select A.comprobante, A.Fecha_pago, 0 D, 0 C ,A.debito, A.credito
     from PagFin123 B
      Right Join (Select comprobante, Fecha_pago, Sum(debito) debito, Sum(credito) credito
     from PagAtl123
     Group by comprobante, Fecha_pago) A    
      on A.Comprobante = B.Comprobante
    and A.Fecha_Pago = B.Fecha_Pago
     Where B.Comprobante is null
    );

   -- /////// SE IINSERTAN LOS REGISTROS QUE ESTAN EN ATLANTIS QUE NO ESTAN EN FINANCOOP           
     Insert into PagFin123(Comprobante, Fecha_Pago, Debito, Credito, Debito_Atlantis, Credito_Atlantis)
     Select * from PagFin245;  
     
     Update  PagFin123
   Inner Join  Cartera  C
    On Comprobante = concat(trim(CarCMTipCom), trim(CarCMPrefijo), CarCMNumCom)
    set Debito_Atlantis = 0, Credito_Atlantis = 0
     Where FecDesCre Between FechaInicio and FechaFinal and CarCMNumCom > 0  and EstCre = 'C' and Debito = 0 and credito = 0;

     Update  PagFin123
   Inner Join  Cartera  C
    On Comprobante = concat(trim(CarAnuTipCom), trim(CarAnuPrefijo), CarAnuNumCom)
    set Debito_Atlantis = 0, Credito_Atlantis = 0
     Where FecDesCre Between FechaInicio and FechaFinal and CarAnuNumCom > 0  and EstCre = 'C' and Debito = 0 and credito = 0;            
     
     IF NombreReporte = 'RMOVDIA' THEN
    INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `TAUsuCon`)
    SELECT 
      @ID := @ID + 1 ID, FECHA_MOVIMIENTO_T, DEBITOS_CONTABILIDAD_N, CREDITOS_CONTABILIDAD_N, DEBITOS_ATLANTIS_N, CREDITOS_ATLANTIS_N, DIFERENCIAS_DEBITOS_N, DIFERENCIAS_CREDITOS_N, Usuario

    FROM (
      SELECT
     @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)), FECHA_MOVIMIENTO_T, DEBITOS_CONTABILIDAD_N, CREDITOS_CONTABILIDAD_N, DEBITOS_ATLANTIS_N, CREDITOS_ATLANTIS_N, DIFERENCIAS_DEBITOS_N, DIFERENCIAS_CREDITOS_N

      FROM (
     SELECT 'FECHA_MOVIMIENTO_T', 'DEBITOS_CONTABILIDAD_N', 'CREDITOS_CONTABILIDAD_N', 'DEBITOS_ATLANTIS_N', 'CREDITOS_ATLANTIS_N', 'DIFERENCIAS_DEBITOS_N', 'DIFERENCIAS_CREDITOS_N'
     UNION ALL 
     Select Fecha_Pago,  debito , Credito, DA, CA, DifDeb, DifCre
     From (Select Fecha_Pago,   Sum(debito)   debito, Sum(Credito) Credito, Sum(Debito_Atlantis) DA, Sum(Credito_Atlantis)CA, (Sum(debito) - Sum(Debito_Atlantis) ) DifDeb, (Sum(Credito)- Sum(Credito_Atlantis)) DifCre
       from PagFin123
       Group by  Fecha_Pago )T                               
       
     ) T1
      ) T2;        
     Else
    INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, `TAUsuCon`)
    SELECT 
      @ID := @ID + 1 ID, FECHA_MOVIMIENTO_T, COMPROBANTE_T, DEBITOS_CONTABILIDAD_N, CREDITOS_CONTABILIDAD_N, DEBITOS_ATLANTIS_N, CREDITOS_ATLANTIS_N, DIFERENCIAS_DEBITOS_N, DIFERENCIAS_CREDITOS_N, Usuario

    FROM (
      SELECT
     @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)), FECHA_MOVIMIENTO_T, COMPROBANTE_T, DEBITOS_CONTABILIDAD_N, CREDITOS_CONTABILIDAD_N, DEBITOS_ATLANTIS_N, CREDITOS_ATLANTIS_N, DIFERENCIAS_DEBITOS_N, DIFERENCIAS_CREDITOS_N

      FROM (
     SELECT 'FECHA_MOVIMIENTO_T', 'COMPROBANTE_T','DEBITOS_CONTABILIDAD_N', 'CREDITOS_CONTABILIDAD_N', 'DEBITOS_ATLANTIS_N', 'CREDITOS_ATLANTIS_N', 'DIFERENCIAS_DEBITOS_N', 'DIFERENCIAS_CREDITOS_N'
     UNION ALL 
     Select Fecha_Pago, Comprobante, debito, Credito, DA, CA, DifDeb, DifCre
     From (Select Comprobante,  Fecha_Pago, Sum(debito)   debito, Sum(Credito) Credito, Sum(Debito_Atlantis) DA, Sum(Credito_Atlantis)CA, (Sum(debito) - Sum(Debito_Atlantis) ) DifDeb, (Sum(Credito)- Sum(Credito_Atlantis)) DifCre
       from PagFin123
             -- Where Not ((Sum(debito) - Sum(Debito_Atlantis) ) = 0 and (Sum(Credito)- Sum(Credito_Atlantis))  = 0)
       Group by Fecha_Pago, Comprobante)T
     ) T1
      ) T2;          
     END IF;
    
   drop table PagAtl123;
   drop table PagFin123;
   drop table PagFin245;             
   END IF;
 
IF NombreReporte = 'RPTCOMPCRE' THEN -- /*REPORTE COMPROMISOS CRÉDITOS, CRÉDITOS APROBADOS PENDIENTES POR DESEMBOLSAR*/
		INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
									  `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
									  `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
									  `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
									  `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
									  `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
									  `Col49`, `Col50`,`TAUsuCon`)
		SELECT 	@ID := @ID + 1 ID,
				`No_N`,`NOMBRESYAPELLIDOS_T`,`C.C_T`,`TELEFONOCELULAR_T`,`TIPODEASOCIADO_T`,`FECHADEAFILIACION_T`,`VALORAPORTESALAFECHA_N`,
				`MUNICIPIO_T`,`VALORSOLICITADO_N`,`NoCUOTAS_T`,`TIPODECRÉDITO_T`,`NOMBRECODEUDOR1_T`, `NOMBRECODEUDOR2_T`,`OBSERVACIONES_T`,
				'(NULL)15','(NULL)16','(NULL)17','(NULL)18','(NULL)19','(NULL)20','(NULL)21','(NULL)22','(NULL)23','(NULL)24',
				'(NULL)25','(NULL)26', '(NULL)27', '(NULL)28', '(NULL)29', '(NULL)30', '(NULL)31', '(NULL)32', '(NULL)33', 
				'(NULL)34', '(NULL)35', '(NULL)36', '(NULL)37', '(NULL)38', '(NULL)39', '(NULL)40','(NULL)41', '(NULL)42', 
				'(NULL)43', '(NULL)44', '(NULL)45', '(NULL)46', '(NULL)47', '(NULL)48','(NULL)49', '(NULL)50', Usuario
		FROM (
				
				SELECT 	@ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
						`No_N`,`NOMBRESYAPELLIDOS_T`,`C.C_T`,`TELEFONOCELULAR_T`,`TIPODEASOCIADO_T`,`FECHADEAFILIACION_T`,`VALORAPORTESALAFECHA_N`,
						`MUNICIPIO_T`,`VALORSOLICITADO_N`,`NoCUOTAS_T`,`TIPODECRÉDITO_T`,`NOMBRECODEUDOR1_T`, `NOMBRECODEUDOR2_T`,`OBSERVACIONES_T`
				FROM (
						(
						SELECT 	'No_N' `No_N`,'NOMBRESYAPELLIDOS_T' `NOMBRESYAPELLIDOS_T`,'C.C_T' `C.C_T`,'TELEFONOCELULAR_T' `TELEFONOCELULAR_T`,
						'TIPODEASOCIADO_T' `TIPODEASOCIADO_T`,'FECHADEAFILIACION_T' `FECHADEAFILIACION_T`,'VALORAPORTESALAFECHA_N' `VALORAPORTESALAFECHA_N`,
						'MUNICIPIO_T' `MUNICIPIO_T`,'VALORSOLICITADO_N' `VALORSOLICITADO_N`,'NoCUOTAS_T' `NoCUOTAS_T`,'TIPODECRÉDITO_T' `TIPODECRÉDITO_T`,
						'NOMBRECODEUDOR1_T' `NOMBRECODEUDOR1_T`,'NOMBRECODEUDOR2_T' `NOMBRECODEUDOR2_T`,'OBSERVACIONES_T' `OBSERVACIONES_T`
						)
						UNION ALL
						(
						/*COMPROMISOS CRÉDITOS*/
						SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)) `No_N`,
						CONCAT(IFNULL(a.asoprinom,''),' ',IFNULL(a.asosegnom,''),' ',IFNULL(a.asopriape,''),' ',IFNULL(a.asosegape,'')) `NOMBRESYAPELLIDOS_T`,
						a.AsoIde `C.C_T`,a.Asocel `TELEFONOCELULAR_T`,cxp.CatProDes `TIPODEASOCIADO_T`,a.AsoFecApr `FECHADEAFILIACION_T`,mc.MCtaSalAct `VALORAPORTESALAFECHA_N`,
						m.MunNombre `MUNICIPIO_T`,sc.SolCreMon `VALORSOLICITADO_N`,(sc.SolCrePla/sc.SolCrePer) `NoCUOTAS_T`,tc.TipCreDes `TIPODECRÉDITO_T`,
						CONCAT(IFNULL(CA1.asoprinom,''),' ',IFNULL(CA1.asosegnom,''),' ',IFNULL(CA1.asopriape,''),' ',IFNULL(CA1.asosegape,'')) `NOMBRECODEUDOR1_T`,
						CONCAT(IFNULL(CD1.codprinom,''),' ',IFNULL(CD1.codsegnom,''),' ',IFNULL(CD1.codpriape,''),' ',IFNULL(CD1.codsegape,'')) `NOMBRECODEUDOR2_T`,
						c.ObsCre `OBSERVACIONES_T`
						FROM atlantiscoomuldenorteNiif.solcre sc INNER JOIN atlantiscoomuldenorteNiif.tipocreditos tc ON tc.TipCreCod=sc.TipCreCod 
							INNER JOIN atlantiscoomuldenorteNiif.asociados a ON a.AsoCod=sc.AsoCod
							INNER JOIN (Select CatProCod,CatProDes From atlantiscoomuldenorteNiif.categoriaxproceso Where CatCod=1) cxp ON cxp.CatProCod=a.AsoTipFor 
							INNER JOIN atlantiscoomuldenorteNiif.municipios m ON m.MunCod=a.AsoLugRes 
							INNER JOIN atlantiscoomuldenorteNiif.cartera c ON sc.solcrecod = c.solcrecod and c.estcre = 'P'
							LEFT JOIN solcrecod scc ON scc.SolCreCod=sc.SolCreCod 
							LEFT JOIN (Select AsoCod,MCtaSalAct From atlantiscoomuldenorteNiif.maestrocuentas Where SerCod='001') mc ON mc.AsoCod=a.AsoCod
							LEFT JOIN (Select * From atlantiscoomuldenorteNiif.Asociados) CA1 On SolCreCodId=CA1.AsoCod AND scc.SolCreCODTip='A'
							LEFT JOIN (Select * From atlantiscoomuldenorteNiif.Codeudor) CD1 On  SolCreCodId=CD1.CodCod AND scc.SolCreCODTip='C'
						WHERE sc.SolCreEst='A' 
						GROUP BY a.asocod
						)
					) T1
			) T1;
	END IF;
		
	IF NombreReporte = 'RPTSOLCRE' THEN	-- REPORTES SOLICITUDES DE CRÉDITO POR APROBAR
		INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
									  `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
									  `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
									  `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
									  `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
									  `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
									  `Col49`, `Col50`,`TAUsuCon`)
		SELECT 	@ID := @ID + 1 ID,
				`No_N`,`NOMBRESYAPELLIDOS_T`,`C.C_T`,`TELEFONOCELULAR_T`,`TIPODEASOCIADO_T`,`FECHADEAFILIACION_T`,`VALORAPORTESALAFECHA_N`,
				`MUNICIPIO_T`,`VALORSOLICITADO_N`,`NoCUOTAS_T`,`TIPODECRÉDITO_T`,`NOMBRECODEUDOR1_T`, `NOMBRECODEUDOR2_T`,`OBSERVACIONES_T`,
				'(NULL)15','(NULL)16','(NULL)17','(NULL)18','(NULL)19','(NULL)20','(NULL)21','(NULL)22','(NULL)23','(NULL)24',
				'(NULL)25','(NULL)26', '(NULL)27', '(NULL)28', '(NULL)29', '(NULL)30', '(NULL)31', '(NULL)32', '(NULL)33', 
				'(NULL)34', '(NULL)35', '(NULL)36', '(NULL)37', '(NULL)38', '(NULL)39', '(NULL)40','(NULL)41', '(NULL)42', 
				'(NULL)43', '(NULL)44', '(NULL)45', '(NULL)46', '(NULL)47', '(NULL)48','(NULL)49', '(NULL)50', Usuario
		FROM (
				
				SELECT 	@ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
						`No_N`,`NOMBRESYAPELLIDOS_T`,`C.C_T`,`TELEFONOCELULAR_T`,`TIPODEASOCIADO_T`,`FECHADEAFILIACION_T`,`VALORAPORTESALAFECHA_N`,
						`MUNICIPIO_T`,`VALORSOLICITADO_N`,`NoCUOTAS_T`,`TIPODECRÉDITO_T`,`NOMBRECODEUDOR1_T`, `NOMBRECODEUDOR2_T`,`OBSERVACIONES_T`
				FROM (
						(
						SELECT 	'No_N' `No_N`,'NOMBRESYAPELLIDOS_T' `NOMBRESYAPELLIDOS_T`,'C.C_T' `C.C_T`,'TELEFONOCELULAR_T' `TELEFONOCELULAR_T`,
						'TIPODEASOCIADO_T' `TIPODEASOCIADO_T`,'FECHADEAFILIACION_T' `FECHADEAFILIACION_T`,'VALORAPORTESALAFECHA' `VALORAPORTESALAFECHA_N`,
						'MUNICIPIO_T' `MUNICIPIO_T`,'VALORSOLICITADO' `VALORSOLICITADO_N`,'NoCUOTAS_T' `NoCUOTAS_T`,'TIPODECRÉDITO_T' `TIPODECRÉDITO_T`,
						'NOMBRECODEUDOR1_T' `NOMBRECODEUDOR1_T`,'NOMBRECODEUDOR2_T' `NOMBRECODEUDOR2_T`,'OBSERVACIONES_T' `OBSERVACIONES_T`
						)
						UNION ALL
						(
						/*SOLICITUDES DE CRÉDITOS*/
						SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)) `No_N`,
						CONCAT(IFNULL(a.asoprinom,''),' ',IFNULL(a.asosegnom,''),' ',IFNULL(a.asopriape,''),' ',IFNULL(a.asosegape,'')) `NOMBRESYAPELLIDOS_T`,
						a.AsoIde `C.C_T`,a.Asocel `TELEFONOCELULAR_T`,cxp.CatProDes `TIPODEASOCIADO_T`,a.AsoFecApr `FECHADEAFILIACION_T`,mc.MCtaSalAct `VALORAPORTESALAFECHA_N`,
						m.MunNombre `MUNICIPIO_T`,sc.SolCreMon `VALORSOLICITADO_N`,(sc.SolCrePla/sc.SolCrePer) `NoCUOTAS_T`,tc.TipCreDes `TIPODECRÉDITO_T`,
						CONCAT(IFNULL(CA1.asoprinom,''),' ',IFNULL(CA1.asosegnom,''),' ',IFNULL(CA1.asopriape,''),' ',IFNULL(CA1.asosegape,'')) `NOMBRECODEUDOR1_T`,
						CONCAT(IFNULL(CD1.codprinom,''),' ',IFNULL(CD1.codsegnom,''),' ',IFNULL(CD1.codpriape,''),' ',IFNULL(CD1.codsegape,'')) `NOMBRECODEUDOR2_T`,
						sco.SolCreOBSDes `OBSERVACIONES_T`
						FROM atlantiscoomuldenorteNiif.solcre sc INNER JOIN atlantiscoomuldenorteNiif.tipocreditos tc ON tc.TipCreCod=sc.TipCreCod 
							INNER JOIN atlantiscoomuldenorteNiif.asociados a ON a.AsoCod=sc.AsoCod
							INNER JOIN (Select CatProCod,CatProDes From atlantiscoomuldenorteNiif.categoriaxproceso Where CatCod=1) cxp ON cxp.CatProCod=a.AsoTipFor 
							INNER JOIN atlantiscoomuldenorteNiif.municipios m ON m.MunCod=a.AsoLugRes 
							LEFT JOIN solcreobs sco ON sco.SolCreCod=sc.SolCreCod
							LEFT JOIN solcrecod scc ON scc.SolCreCod=sc.SolCreCod
							LEFT JOIN (Select AsoCod,MCtaSalAct From atlantiscoomuldenorteNiif.maestrocuentas Where SerCod='001') mc ON mc.AsoCod=a.AsoCod
							LEFT JOIN (Select * From atlantiscoomuldenorteNiif.Asociados) CA1 On SolCreCodId=CA1.AsoCod AND scc.SolCreCODTip='A'
							LEFT JOIN (Select * From atlantiscoomuldenorteNiif.Codeudor) CD1 On  SolCreCodId=CD1.CodCod AND scc.SolCreCODTip='C'
						WHERE sc.SolCreEst='P'
						GROUP BY a.asocod
						)
					) T1
			) T1;
	END IF;  

	IF NombreReporte = 'RPTCREDESE' THEN -- /*REPORTE CRÉDITOS DESEMBOLSADOS,CONSOLIDADO CRÉDITOS DESEMBOLSADOS*/
		INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
									  `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
									  `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
									  `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
									  `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
									  `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
									  `Col49`, `Col50`,`TAUsuCon`)
		SELECT 	@ID := @ID + 1 ID,
				`No_N`,`NOMBRESYAPELLIDOS_T`,`C.C_T`,`TELEFONOCELULAR_T`,`TIPODEASOCIADO_T`,`MUNICIPIO_T`,`VALORDESEMBOLSADO_N`,
				`TIPODECRÉDITO_T`,`NOMBRECODEUDOR1_T`, `NOMBRECODEUDOR2_T`,`FECHASOLICITUD_T`,`FECHADESEMBOLSO_T`,
				`LINEADECREDITO_T`,`OBSERVACIONES_T`,'(NULL)15','(NULL)16','(NULL)17','(NULL)18','(NULL)19','(NULL)20',
					 '(NULL)21','(NULL)22','(NULL)23','(NULL)24','(NULL)25','(NULL)26','(NULL)27','(NULL)28',
					 '(NULL)29','(NULL)30','(NULL)31','(NULL)32','(NULL)33','(NULL)34','(NULL)35','(NULL)36', 
					 '(NULL)37','(NULL)38','(NULL)39','(NULL)40','(NULL)41','(NULL)42','(NULL)43','(NULL)44',
					 '(NULL)45','(NULL)46','(NULL)47','(NULL)48','(NULL)49','(NULL)50', Usuario
		FROM (
				SELECT 	@ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
						`No_N`,`NOMBRESYAPELLIDOS_T`,`C.C_T`,`TELEFONOCELULAR_T`,`TIPODEASOCIADO_T`,`MUNICIPIO_T`,`VALORDESEMBOLSADO_N`,
						`TIPODECRÉDITO_T`,`NOMBRECODEUDOR1_T`, `NOMBRECODEUDOR2_T`,`FECHASOLICITUD_T`,`FECHADESEMBOLSO_T`,`LINEADECREDITO_T`,
						`OBSERVACIONES_T`
				FROM (
						(
						SELECT 	'No_N' `No_N`,'NOMBRESYAPELLIDOS_T' `NOMBRESYAPELLIDOS_T`,'C.C_T' `C.C_T`,'TELEFONOCELULAR_T' `TELEFONOCELULAR_T`,
						'TIPODEASOCIADO_T' `TIPODEASOCIADO_T`,'MUNICIPIO_T' `MUNICIPIO_T`,'VALORDESEMBOLSADO_N' `VALORDESEMBOLSADO_N`,
						'TIPODECRÉDITO_T' `TIPODECRÉDITO_T`,'NOMBRECODEUDOR1_T' `NOMBRECODEUDOR1_T`,'NOMBRECODEUDOR2_T' `NOMBRECODEUDOR2_T`,
						'FECHASOLICITUD_T' `FECHASOLICITUD_T`,'FECHADESEMBOLSO_T' `FECHADESEMBOLSO_T`,'LINEADECREDITO_T' `LINEADECREDITO_T`,'OBSERVACIONES_T' `OBSERVACIONES_T`
						)
						UNION ALL
						(
						/*CRÉDITOS DESEMBOLSADOS*/
						SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)) `No_N`,
						CONCAT( IFNULL(a.asopriape,''),' ',IFNULL(a.asosegape,''),' ',IFNULL(a.asoprinom,''),' ',IFNULL(a.asosegnom,'')) `NOMBRESYAPELLIDOS_T`,
						a.AsoIde `C.C_T`,a.Asocel `TELEFONOCELULAR_T`,cxp.CatProDes `TIPODEASOCIADO_T`,m.MunNombre `MUNICIPIO_T`,sc.SolCreMon `VALORDESEMBOLSADO_N`,
						tc.TipCreDes `TIPODECRÉDITO_T`,CONCAT(IFNULL(CA1.asoprinom,''),' ',IFNULL(CA1.asosegnom,''),' ',IFNULL(CA1.asopriape,''),' ',IFNULL(CA1.asosegape,'')) `NOMBRECODEUDOR1_T`,
						CONCAT(IFNULL(CD1.codprinom,''),' ',IFNULL(CD1.codsegnom,''),' ',IFNULL(CD1.codpriape,''),' ',IFNULL(CD1.codsegape,'')) `NOMBRECODEUDOR2_T`,
						sc.solcrefre `FECHASOLICITUD_T`,c.fecdescre `FECHADESEMBOLSO_T`,lc.Lincredes `LINEADECREDITO_T`,c.ObsCre `OBSERVACIONES_T`
						FROM atlantiscoomuldenorteNiif.solcre sc INNER JOIN atlantiscoomuldenorteNiif.tipocreditos tc ON tc.TipCreCod=sc.TipCreCod 
							INNER JOIN atlantiscoomuldenorteNiif.asociados a ON a.AsoCod=sc.AsoCod
							INNER JOIN (Select CatProCod,CatProDes From atlantiscoomuldenorteNiif.categoriaxproceso Where CatCod=1) cxp ON cxp.CatProCod=a.AsoTipFor 
							INNER JOIN atlantiscoomuldenorteNiif.municipios m ON m.MunCod=a.AsoLugRes 
							INNER JOIN atlantiscoomuldenorteNiif.lineacredito lc ON lc.lincrecod=Sc.lincrecod -- cambio para agregar linea de credito
							INNER JOIN atlantiscoomuldenorteNiif.cartera c ON sc.solcrecod = c.solcrecod AND c.fecdescre >= FechaInicio AND c.fecdescre <= FechaFinal
							LEFT JOIN solcrecod scc ON scc.SolCreCod=sc.SolCreCod 
							LEFT JOIN (Select * From atlantiscoomuldenorteNiif.Asociados) CA1 On SolCreCodId=CA1.AsoCod AND scc.SolCreCODTip='A'
							LEFT JOIN (Select * From atlantiscoomuldenorteNiif.Codeudor) CD1 On  SolCreCodId=CD1.CodCod AND scc.SolCreCODTip='C'
						WHERE sc.SolCreEst='C' 
						GROUP BY a.asocod
						)
					) T1
			) T1;
	END IF;    
    
IF NombreReporte = 'PRECIECAR' THEN   -- PRECIERRE DE CARTERA
	  SELECT ESTPARVAL 
	  INTO TASAMORA
	  FROM ESTATUTOS 
	  WHERE ESTPARCOD = 'INTMORA';

	  INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
			 `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
			 `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
			 `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
			 `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
			 `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
			 `Col49`, `Col50`,`TAUsuCon`)
	  SELECT  @ID := @ID + 1 ID,
		`CARASOCOD`,`IDENTIFICACION`,`ASOCIADO`,
		`CARCOD`,`TIPCRECOD`,`TIPCREVARCOD`,`ESTADO`,
		`SALDOACTUAL`,`SALDOINSOLUTO`,`DIASMORA`,`PORPARTICIPA`,`EDADCARTERA`,`EDADCARTERADC`,
		`EDADCARTERAAR`, `PORGARANTIA`,`PORPROVISION`,`VALGARANTIA`,`VALPROVISION`,`NUMDIASINTERESESCAUSADOS`,
		`INTCAUSADOS`,`NUMDIASINTCONTINGENTES`,`INTCONTINGENTES`,`PORPROVISIONINTERESES`,`VALPROVISIONINTERESES`,`FECHACIERRE`,
		`FECHAREGISTRO`,`USUARIOREGISTRA`,`DESCRIPCION`, `ALTURA`, `SALDOINTERESES`, `OTROS`, 
		`CAPITALMORA`, `FECMAXPAG`, `VALORCUOTA`, `PLAZO`, `FORMAPAGO`, `TIPOGARANTIA`, `TASA`, `INTERESMORA`, `CUOTASMORA`,
		`FECHAPRIMERACUOTA`, `FECHAVENCIMIENTO`, `CORTOPLAZO`, `LARGOPLAZO`, `PORSEGURO`, `FECHA_DESEMBOLSO`, `CUENTACONTABLE`, `VALORDESEMBOLSO`,
		`SUCURSALCREDITO`, '(NULL)50', Usuario
	  FROM (
		
		SELECT  @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
		  `CARASOCOD`,`IDENTIFICACION`,`ASOCIADO`,
		  `CARCOD`,`TIPCRECOD`,`TIPCREVARCOD`,`ESTADO`,
		  `SALDOACTUAL`,`SALDOINSOLUTO`,`DIASMORA`,`PORPARTICIPA`,`EDADCARTERA`,`EDADCARTERADC`,
		  `EDADCARTERAAR`, `PORGARANTIA`,`PORPROVISION`,`VALGARANTIA`,`VALPROVISION`,`NUMDIASINTERESESCAUSADOS`,
		  `INTCAUSADOS`,`NUMDIASINTCONTINGENTES`,`INTCONTINGENTES`,`PORPROVISIONINTERESES`,`VALPROVISIONINTERESES`,`FECHACIERRE`,
		  `FECHAREGISTRO`,`USUARIOREGISTRA`,`DESCRIPCION`, `ALTURA`, `SALDOINTERESES`, `OTROS`, 
		  `CAPITALMORA`, `FECMAXPAG`, `VALORCUOTA`, `PLAZO`, `FORMAPAGO`, `TIPOGARANTIA`, `TASA`, `INTERESMORA`, `CUOTASMORA`,
		  `FECHAPRIMERACUOTA`, `FECHAVENCIMIENTO`, `CORTOPLAZO`, `LARGOPLAZO`, `PORSEGURO`, `FECHA_DESEMBOLSO`, `CUENTACONTABLE`, `VALORDESEMBOLSO`, `SUCURSALCREDITO`
		FROM (
		  (
		  SELECT  'CARASOCOD_N' `CARASOCOD`, 'IDENTIFICACION_T' `IDENTIFICACION`, 'ASOCIADO_T' `ASOCIADO`,
			'CARCOD_T' `CARCOD`,'TIPCRECOD_N' `TIPCRECOD`,'TIPCREVARCOD_N' `TIPCREVARCOD`,'ESTADO_T' `ESTADO`,
			'SALDOACTUAL_N' `SALDOACTUAL`,'SALDOINSOLUTO_N' `SALDOINSOLUTO`,'DIASMORA_N' `DIASMORA`,'PORPARTICIPA_N' `PORPARTICIPA`,'EDAD CARTERA CORTE ANTERIOR_T' `EDADCARTERA`,'EDAD CARTERA AL CORTE_T' `EDADCARTERADC`,
			'EDAD CARTERA ARRASTRE_T' `EDADCARTERAAR`, 'PORGARANTIA_N' `PORGARANTIA`,'PORPROVISION_N' `PORPROVISION`,'VALGARANTIA_N' `VALGARANTIA`,'VALPROVISION_N' `VALPROVISION`,'NUMDIASINTERESESCAUSADOS_N' `NUMDIASINTERESESCAUSADOS`,
			'INTCAUSADOS_N' `INTCAUSADOS`,'NUMDIASINTCONTINGENTES_N' `NUMDIASINTCONTINGENTES`,'INTCONTINGENTES_N' `INTCONTINGENTES`,'PORPROVISIONINTERESES_N' `PORPROVISIONINTERESES`,'VALPROVISIONINTERESES_N' `VALPROVISIONINTERESES`,'FECHACIERRE_D' `FECHACIERRE`,
			'FECHAREGISTRO_D' `FECHAREGISTRO`,'USUARIOREGISTRA_T' `USUARIOREGISTRA`,'DESCRIPCION_T' `DESCRIPCION`,'ALTURA_N' `ALTURA`, 'SALDOINTERESES_N' `SALDOINTERESES`, 'OTROS_N' `OTROS`, 
			'CAPITALMORA_N' `CAPITALMORA`, 'FECMAXPAG_D' `FECMAXPAG`, 'VALORCUOTA_N' `VALORCUOTA`, 'PLAZO_N' `PLAZO`, 'FORMAPAGO_T' `FORMAPAGO`, 'TIPOGARANTIA_T' `TIPOGARANTIA`, 'TASA_N' `TASA`, 'INTERESMORA_N' `INTERESMORA`, 'CUOTASMORA_N' `CUOTASMORA`,
			'FECHAPRIMERACUOTA_T' `FECHAPRIMERACUOTA`, 'FECHAVENCIMIENTO_T'  `FECHAVENCIMIENTO`, 'CORTOPLAZO_N' `CORTOPLAZO`, 'LARGOPLAZO_N' `LARGOPLAZO`, 'PORSEGURO_T' `PORSEGURO`, 'FECHA_DESEMBOLSO_T' `FECHA_DESEMBOLSO`, 'CUENTACONTABLE_N' `CUENTACONTABLE`, 'VALORDESEMBOLSO_N' `VALORDESEMBOLSO`,  'SUCURSALCREDITO_T' `SUCURSALCREDITO`
		  )
		  UNION ALL
		  (
		  SELECT  pCieCarAsoCod `CARASOCOD`, T2.ASOIDE `IDENTIFICACION`, CONCAT(IFNULL(T2.ASOPRINOM, ''),' ',IFNULL(T2.ASOSEGNOM, ''),' ',IFNULL(T2.ASOPRIAPE,''),' ',IFNULL(T2.ASOSEGAPE,'')) `ASOCIADO`,
			pCieCarCarCod `CARCOD`,pCieCarTipCre `TIPCRECOD`,pCieCarTipCreVar `TIPCREVARCOD`, pCieCarEst `ESTADO`,
			TRUNCATE(pCieCarSalAct,0) `SALDOACTUAL`,pCieCarSalIns `SALDOINSOLUTO`,pCieCarDiaMor `DIASMORA`,pCieCarPorPar `PORPARTICIPA`,pCieCarEXCAC `EDADCARTERA`,pCieCarEXCDC `EDADCARTERADC`,
			pCieCarEXCAR `EDADCARTERAAR`,pCieCarPorGar `PORGARANTIA`,pCieCarPorPro `PORPROVISION`,pCieCarValGar `VALGARANTIA`,pCieCarValPro `VALPROVISION`,pCieCarNDICa `NUMDIASINTERESESCAUSADOS`,
			pCieCarIntCau `INTCAUSADOS`,pCieCarNDICo `NUMDIASINTCONTINGENTES`,pCieCarIntCon `INTCONTINGENTES`,pCieCarPorPIn `PORPROVISIONINTERESES`,pCieCarValPIn`VALPROVISIONINTERESES`,pCieCarFecCie `FECHACIERRE`,
			pCieCarFecReg `FECHAREGISTRO`,pCieCarUsuReg `USUARIOREGISTRA`,If(T3.COMPROBANTEDESEMBOLSO is null or T3.COMPROBANTEDESEMBOLSO = '',  Concat(trim(T3.CarCMTipCom), Trim(T3.CarCMPrefijo), T3.CarCMNumCom), T3.COMPROBANTEDESEMBOLSO) 
									
									
									`DESCRIPCION`,pCieCarAltCre `ALTURA`,pCieCarSalActInt `SALDOINTERESES`,pCieCarSalOtr`OTROS`, 
			pCieCarValCapMor `CAPITALMORA`,pCieCarFecUltPag `FECMAXPAG`, T3.VALCUO `VALORCUOTA`, (T4.SOLCREPLA/T4.SOLCREPER) `PLAZO`, IF(TipCreVarFPa = 1, 'Con Libranza (Nómina)', 'Sin Libranza (Caja)') `FORMAPAGO`, 
			IF(TipCreVarTGa = 1, 'Admisible', 'Otras Garantías') `TIPOGARANTIA`, T4.SOLCRETAS `TASA`, ROUND(((T1.PCIECARSALACT * 12 * TASAMORA * PCIECARDIAMOR)/36000),0) `INTERESMORA`, 
			IF((TIMESTAMPDIFF(MONTH,pCieCarFecUltPag,DATE_ADD(FechaFinal, INTERVAL -1 MONTH))) < 0, 0, TIMESTAMPDIFF(MONTH,pCieCarFecUltPag,DATE_ADD(FechaFinal, INTERVAL -1 MONTH)))  `CUOTASMORA`,
			FECPRICUO `FECHAPRIMERACUOTA`, DATE_ADD( FECPRICUO, INTERVAL (SOLCREPLA -1) MONTH)  `FECHAVENCIMIENTO`, `pCieCarCorPla` `CORTOPLAZO`, `pCieCarLarPla` `LARGOPLAZO`, `pCieCarValSeg` `PORSEGURO`,
			FECDESCRE `FECHA_DESEMBOLSO`, PCIECARCTAPUC `CUENTACONTABLE`, pCieCarValDes `VALORDESEMBOLSO`, PCieCarSucCre `SUCURSALCREDITO`
		  FROM PRECIECAR T1 INNER JOIN ASOCIADOS T2 ON T1.pCieCarAsoCod = T2.ASOCOD
			   INNER JOIN CARTERA T3 ON T1.pCIECARCARCOD = T3.CARCOD
			   INNER JOIN SOLCRE T4 ON T3.SOLCRECOD = T4.SOLCRECOD
			   INNER JOIN TIPOCREDITOSVARIABLE T5 ON T4.TIPCRECOD = T5.TIPCRECOD AND T4.TIPCREVARCOD = T5.TIPCREVARCOD
		  WHERE pCIECARFECCIE = FechaFinal

		  )
		 ) T1
	   ) T1;
 END IF; 
 
IF NombreReporte = 'RDIFCRECIE' THEN        
   
        drop table if exists Credito234;
        drop table if exists AboCar234;

        -- /////// TEMPORAL PARA ALMACENAR LOS PAGOS DESDE ATLANTIS DEL PERIODO INGRESADO
        CREATE TEMPORARY TABLE Credito234 AS
				(
					SELECT CREDITO
								  FROM(  SELECT T1.CIECARCARCOD CREDITO, ASOIDE IDENTIFICACION, T1.CIECARSALACT SALDO_CIERRE_MARZO, T3.CR ABONOS_A_HOY, T1.CIECARSALACT - T3.CR SALDO_A_HOY, SALACTCRE SALDO_ATLANTIS,  T1.CIECARSALACT - T3.CR - SALACTCRE DIFERENCIA_SALDOS
											FROM CIECAR T1 INNER JOIN (SELECT TC.CARCOD,  IFNULL(CR,0) CR, SALACTCRE
																		FROM CARTERA  TC
																			LEFT JOIN (SELECT CARCOD, SUM(K) CR
																							FROM (
																									-- TRAE ABONOS REALIZADOS EN LA NOMINA
																									SELECT CARCOD, Sum(AVALDISCAP) K, CONCAT(APAGCARTIPCOM, trim(APAGCARPREFIJO), APAGCARNUMCOM) COMPROBANTE, 'N' TIPO
																									FROM (
																											SELECT T1.CARCOD, AVALDISCAP,  AFECREAPAG, T1.APAGCARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM
																											FROM PAGOSCARTERAA T1 INNER JOIN CARTERANOMINAPAGOS T2 ON T1.APAGCARCOD = T2.APAGCARCOD
																																	INNER JOIN CARTERANOMINA T3 ON T2.CARNOMCOD = T3.CARNOMCOD										
																											WHERE APAGCARTIPPAG = 'N' AND CARNOMPER >= (
																																						YEAR(DATE_ADD((SELECT MAX(CIECARFECCIE)FROM CIECAR), INTERVAL 1 DAY)) * 100 + MONTH(DATE_ADD((SELECT MAX(CIECARFECCIE)FROM CIECAR), INTERVAL 1 DAY))
																																					  )
																										 ) T1 
																									GROUP BY CARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM
																									UNION ALL
																									-- TRAE ABONOS REALIZADOS POR LA FORMA DE PAGO "PAGO DE CRÉDITOS" QUE NO FUERON POR NOMINA Y TAMPOCO Q FUERON POR DESAFILIACION
																									-- HAY Q ESTAR PENDIENTE POR Q ESTA QUEMADO EL COMPROBANTE CONTABLE DE DESAFILIACION
																									SELECT CARCOD, Sum(aValDisCap) K, CONCAT(APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM) COMPROBANTE, 'C' TIPO
																									FROM (
																											SELECT CARCOD, AVALDISCAP,  AFECREAPAG, APAGCARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM, ANUMPAG
																											FROM PAGOSCARTERAA T1 LEFT JOIN (
																																				SELECT T2.AFPNUMPAG, T2.FORPAGCOD
																																				FROM PAGOSCARTERAA T1 INNER JOIN FORMASPAGOCARTERAA T2 ON T1.ANUMPAG = T2.AFPNUMPAG
																																				WHERE T2.FORPAGCOD = 8
																																				GROUP BY T2.AFPNUMPAG
																																			) T2 ON T1.ANUMPAG = T2.AFPNUMPAG
																											WHERE AFECREAPAG >= DATE_ADD((SELECT MAX(CIECARFECCIE)FROM CIECAR), INTERVAL 1 DAY) AND AFECREAPAG <= CURDATE() AND APAGCARTIPPAG <> 'N'
																														AND (T2.AFPNUMPAG IS NULL OR T2.AFPNUMPAG = 0)																														
																										) T1 
																									GROUP BY CARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM 
																								) T1
																					GROUP BY CARCOD ) TP
																			ON TC.CARCOD = TP.CARCOD 
																		WHERE ESTCRE <> 'C') T3
																	ON T1.CIECARCARCOD = T3.CARCOD AND CIECARFECCIE = (SELECT MAX(CIECARFECCIE)FROM CIECAR)
															 LEFT JOIN ASOCIADOS T4 ON T1.CIECARASOCOD = T4.ASOCOD  
											WHERE  T1.CIECARSALACT - T3.CR <> SALACTCRE  ) T5
									
									LEFT JOIN (Select aCarCodAnu, ValorReverso
													from bdfinancoop.transacciones R 
														 Inner Join	(Select R.*, TransaccionFechaDoc FecInicial
																	from bdfinancoop.transacciones 
																		Inner Join	(  Select aCarCodAnu, Concat(aTipComAnu,rtrim(aPrefijoAnu),aNumComAnu) Original, Concat(aPagCarTipComAnu,rtrim(aPagCarPrefijoAnu),aPagCarNumComAnu)  Reversion, sum(aValCapPagAnu) ValorReverso
																						from  pagoscarteraanua P
																						where AfecAnu > (SELECT MAX(CIECARFECCIE)FROM CIECAR)  and aPagCarTipComAnu in ('R', 'M')
																						Group by aPagCarTipComAnu, aCarCodAnu, aTipComAnu, aPrefijoAnu, aNumComAnu, aPagCarTipComAnu, aPagCarPrefijoAnu, aPagCarNumComAnu) R
																			on transaccionDocCon = Original) O
															 on transaccionDocCon = Reversion 
													Where Not (year(FecInicial) = year(TransaccionFechaDoc) and month(FecInicial) = month(TransaccionFechaDoc))) T7
										ON  T7.aCarCodAnu = T5.CREDITO
								WHERE NOT(SALDO_A_HOY  +  IFNULL(T7.ValorReverso,0)  = 0 AND SALDO_ATLANTIS = 0)    
                        );
                   
        CREATE TEMPORARY TABLE AboCar234 AS
				(                        
					Select IfNull(CarCod, Credito) CarCod, Concat(aPagCarTipCom, trim(aPagCarPrefijo), aPagcarNumCom)Comprobante, IfNull(aFecMaxPag, '1900-01-01') FecMaxPag, IfNull(aFecReaPag, '1900-01-01') FecReaPag, IfNull(asalcre, 0) SalPag, IfNull(aPagCarTipPag, '') PagCarTipPag, 1 Consulta
					from Credito234 C 
						Left Join pagoscarteraa P
							on P.CarCod = C.Credito
								and aPagCarTipPag not in ('','N')); 
					
					--   Se seleccionan los registros de pago (Tipo N -) para el periodo escogido 
					Insert  AboCar234(CarCod, Comprobante, FecMaxPag, FecReaPag,  SalPag, PagCarTipPag, Consulta)
					Select P.CarCod, Concat(aPagCarTipCom,  trim(aPagCarPrefijo), aPagcarNumCom)Comprobante, aFecMaxPag, Case when aFecReaPag > LAST_DAY(convert(concat(substring(CarNomPer, 1, 4), substring(CarNomPer, 5, 2), '01'), date)) then 
						LAST_DAY(convert(concat(substring(CarNomPer, 1, 4), substring(CarNomPer, 5, 2), '01'), date)) Else aFecReaPag end, asalcre, aPagCarTipPag, 2 Consulta
					from pagoscarteraa P
						Inner Join carteranomina a
							on p.carcod = a.CarCod
						Inner Join carteranominapagos n
							on a.CarNomCod = n.CarNomCod
								and p.aPagCarCod = n.aPagCarCod
						Inner Join Credito234 C
							on P.CarCod = c.Credito                                    
					Where aPagCarTipPag in ('N') ;
                                                    
		INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `TAUsuCon`)
		SELECT 
				@ID := @ID + 1 ID, CREDITO_T, FECHA_DE_CIERRE_T, SALDO_REPORTE_CIERRE_N, SALDOS_EXTRACTO_N, DIFERENCIAS_SALDOS_N, 'ADMIN' Usuario

		FROM (
				SELECT
						@ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)), CREDITO_T, FECHA_DE_CIERRE_T, SALDO_REPORTE_CIERRE_N, SALDOS_EXTRACTO_N, DIFERENCIAS_SALDOS_N

				FROM (
						SELECT 'CREDITO_T', 'FECHA_DE_CIERRE_T', 'SALDO_REPORTE_CIERRE_N', 'SALDOS_EXTRACTO_N', 'DIFERENCIAS_SALDOS_N'
						UNION ALL                        
						Select CarCod, CieCarFecCie, CieCarSalAct, SalPag, CieCarSalAct - SalPag Diferencia
						From ciecar C1
							Inner Join (Select CarCod, PerPagCre,  min(SalPag) SalPag
											From (Select CarCod, LAST_DAY(FecReaPag) PerPagCre, SalPag --  (year(FecReaPag) * 100 + Month(FecReaPag)) Periodo, SalPag
													from AboCar234 A    
													Order by 2)A1
											Group by CarCod, PerPagCre 
											Order by 1,2) S1
						on S1.carcod = C1.ciecarcarcod
						and CieCarFecCie = PerPagCre
					)T1
                )T2;							
        drop table Credito234;
        drop table AboCar234;
END IF; 

If NombreReporte = 'REPROCIFIN' THEN -- HABILITAR CREDITOS PARA REPROCESAR CIFIN
		-- eliminar el registro en historrico de procesos
		delete from hisproc Where HPTipPro = 'CIFI'  And HPFecFin = FechaFinal;

		update cartera set  feccenrie=FechaInicio where carcod in( select ciecarcarcod from ciecar where ciecarfeccie=FechaFinal) ;
		-- Actualizar creditos del cierre anterior que no estan el cierre actual
		update cartera set  feccenrie=FechaInicio where carcod in(
		select ciecarcarcod from ciecar where ciecarfeccie=FechaInicio and ciecarcarcod 
		 not in(select ciecarcarcod from ciecar where ciecarfeccie = FechaFinal and ciecarsalact>10000)) ;
         
-- Dejar fecha central de reisgos en ''01/01/1753'' para que solo se reporte por castigo a los creditos castigados por el sistema
          update cartera set  feccenrie='1753-01-01' where carcod in(select ActCasCarCod from actcas );
          -- actalizar los casigos pagados en emmes par aque aparescan en el reporte
          Update actcas set ActcasFecCenRie = FechaInicio where actcascod in(select actcascod from actcasdet where ActCasDetFec > FechaInicio);
          
End If;

	IF NombreReporte = 'RCRECANSAL' THEN
	  INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `TAUsuCon`)
	  SELECT 
		@ID := @ID + 1 ID, CREDITO_T, IDENTIFICACION_T, SALDO_ULT_CIERRE_N, ABONOS_A_HOY_N, SALDO_A_HOY_N, SALDO_ATLANTIS_N,  DIFERENCIA_SALDOS_N, Usuario

	  FROM (
		SELECT
		  @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)), CREDITO_T, IDENTIFICACION_T, SALDO_ULT_CIERRE_N, ABONOS_A_HOY_N, SALDO_A_HOY_N, SALDO_ATLANTIS_N,  DIFERENCIA_SALDOS_N

		FROM (
		  SELECT 'CREDITO_T', 'IDENTIFICACION_T', 'SALDO_ULT_CIERRE_N', 'ABONOS_A_HOY_N', 'SALDO_A_HOY_N', 'SALDO_ATLANTIS_N',  'DIFERENCIA_SALDOS_N'
		  UNION ALL                    
			SELECT CREDITO, IDENTIFICACION , SALDO_CIERRE_MARZO +  IFNULL(T7.ValorReverso,0) SALDO_CIERRE_MARZO , ABONOS_A_HOY, SALDO_A_HOY  +  IFNULL(T7.ValorReverso,0) SALDO_A_HOY, SALDO_ATLANTIS, IFNULL(T7.ValorReverso,0) + DIFERENCIA_SALDOS  DIFERENCIA_SALDOS
			  FROM(  SELECT T1.CIECARCARCOD CREDITO, ASOIDE IDENTIFICACION, T1.CIECARSALACT SALDO_CIERRE_MARZO, T3.CR ABONOS_A_HOY, T1.CIECARSALACT - T3.CR SALDO_A_HOY, SALACTCRE SALDO_ATLANTIS,  T1.CIECARSALACT - T3.CR - SALACTCRE DIFERENCIA_SALDOS
			   FROM CIECAR T1 INNER JOIN (SELECT TC.CARCOD,  IFNULL(CR,0) CR, SALACTCRE
					  FROM CARTERA  TC
					   LEFT JOIN (SELECT CARCOD, SUM(K) CR
						   FROM (
							 -- TRAE ABONOS REALIZADOS EN LA NOMINA
							 SELECT CARCOD, Sum(AVALDISCAP) K, CONCAT(APAGCARTIPCOM, trim(APAGCARPREFIJO), APAGCARNUMCOM) COMPROBANTE, 'N' TIPO
							 FROM (
							   SELECT T1.CARCOD, AVALDISCAP,  AFECREAPAG, T1.APAGCARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM
							   FROM PAGOSCARTERAA T1 INNER JOIN CARTERANOMINAPAGOS T2 ON T1.APAGCARCOD = T2.APAGCARCOD
									 INNER JOIN CARTERANOMINA T3 ON T2.CARNOMCOD = T3.CARNOMCOD          
							   WHERE APAGCARTIPPAG = 'N' AND CARNOMPER >= (
										  YEAR(DATE_ADD((SELECT MAX(CIECARFECCIE)FROM CIECAR), INTERVAL 1 DAY)) * 100 + MONTH(DATE_ADD((SELECT MAX(CIECARFECCIE)FROM CIECAR), INTERVAL 1 DAY))
										   )
							   ) T1 
							 GROUP BY CARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM
							 UNION ALL
							 -- TRAE ABONOS REALIZADOS POR LA FORMA DE PAGO "PAGO DE CRÉDITOS" QUE NO FUERON POR NOMINA Y TAMPOCO Q FUERON POR DESAFILIACION
							 -- HAY Q ESTAR PENDIENTE POR Q ESTA QUEMADO EL COMPROBANTE CONTABLE DE DESAFILIACION
							 SELECT CARCOD, Sum(aValDisCap) K, CONCAT(APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM) COMPROBANTE, 'C' TIPO
							 FROM (
							   SELECT CARCOD, AVALDISCAP,  AFECREAPAG, APAGCARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM, ANUMPAG
							   FROM PAGOSCARTERAA T1 LEFT JOIN (
										SELECT T2.AFPNUMPAG, T2.FORPAGCOD
										FROM PAGOSCARTERAA T1 INNER JOIN FORMASPAGOCARTERAA T2 ON T1.ANUMPAG = T2.AFPNUMPAG
										WHERE T2.FORPAGCOD = 8
										GROUP BY T2.AFPNUMPAG
									   ) T2 ON T1.ANUMPAG = T2.AFPNUMPAG
							   WHERE AFECREAPAG >= DATE_ADD((SELECT MAX(CIECARFECCIE)FROM CIECAR), INTERVAL 1 DAY) AND AFECREAPAG <= CURDATE() AND APAGCARTIPPAG <> 'N'
								  AND (T2.AFPNUMPAG IS NULL OR T2.AFPNUMPAG = 0)                              
							  ) T1 
							 GROUP BY CARCOD, APAGCARTIPCOM, APAGCARPREFIJO, APAGCARNUMCOM 
							) T1
						 GROUP BY CARCOD ) TP
					   ON TC.CARCOD = TP.CARCOD 
					  WHERE ESTCRE = 'C') T3
					 ON T1.CIECARCARCOD = T3.CARCOD AND CIECARFECCIE = (SELECT MAX(CIECARFECCIE)FROM CIECAR)
					LEFT JOIN ASOCIADOS T4 ON T1.CIECARASOCOD = T4.ASOCOD  
			   WHERE  T1.CIECARSALACT - T3.CR <> SALACTCRE  ) T5
			 
			 LEFT JOIN (Select aCarCodAnu, ValorReverso
				 from mantiscoomuldenorteniif.transacciones R 
				   Inner Join (Select R.*, TransaccionFechaDoc FecInicial
					 from mantiscoomuldenorteniif.transacciones 
					  Inner Join (  Select aCarCodAnu, Concat(aTipComAnu,rtrim(aPrefijoAnu),aNumComAnu) Original, Concat(aPagCarTipComAnu,rtrim(aPagCarPrefijoAnu),aPagCarNumComAnu)  Reversion, sum(aValCapPagAnu) ValorReverso
						  from  pagoscarteraanua P
						  where AfecAnu > (SELECT MAX(CIECARFECCIE)FROM CIECAR)  and aPagCarTipComAnu in ('R', 'M')
						  Group by aPagCarTipComAnu, aCarCodAnu, aTipComAnu, aPrefijoAnu, aNumComAnu, aPagCarTipComAnu, aPagCarPrefijoAnu, aPagCarNumComAnu) R
					   on transaccionDocCon = Original) O
					on transaccionDocCon = Reversion 
				 Where Not (year(FecInicial) = year(TransaccionFechaDoc) and month(FecInicial) = month(TransaccionFechaDoc))) T7
			  ON  T7.aCarCodAnu = T5.CREDITO
			WHERE NOT(SALDO_A_HOY  +  IFNULL(T7.ValorReverso,0)  = 0 AND SALDO_ATLANTIS = 0)) T9
		 )T10;

	END IF;

If NombreReporte = 'CARCRUCONS' THEN	        -- REPORTE CONSIINACIONES NO IDENTIFICADAS VS REGISTRO EN CONSIGNACIONES --
	INSERT INTO TemporalArchivos (`TASec`, `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col6`, `Col7`, `Col8`, 
											  `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16`, 
											  `Col17`, `Col18`, `Col19`, `Col20`, `Col21`, `Col22`, `Col23`, `Col24`, 
											  `Col25`, `Col26`, `Col27`, `Col28`, `Col29`, `Col30`,`Col31`, `Col32`,
											  `Col33`, `Col34`, `Col35`, `Col36`, `Col37`, `Col38`, `Col39`, `Col40`,
											  `Col41`, `Col42`, `Col43`, `Col44`, `Col45`, `Col46`, `Col47`, `Col48`,
											  `Col49`, `Col50`,`TAUsuCon`)
				SELECT 	@ID := @ID + 1 ID,
						`Cuenta_T`, `Documento_T`, `Fecha_T`, `Creditos_N`, `ValorConsig_N`, 
						`Concod_N`, `DocumentoPago_T`, `FechaPago_T`, `Debitos_N`,'(NULL)10', '(NULL)11',
						'(NULL)12', '(NULL)13', '(NULL)14', '(NULL)15', '(NULL)16', '(NULL)17', '(NULL)18',
						'(NULL)19', '(NULL)20', '(NULL)21', '(NULL)22', '(NULL)23', '(NULL)24', '(NULL)25', '(NULL)26',
						'(NULL)27', '(NULL)28', '(NULL)29', '(NULL)30', '(NULL)31', '(NULL)32', '(NULL)33', '(NULL)34',
						'(NULL)35', '(NULL)36', '(NULL)37', '(NULL)38', '(NULL)39', '(NULL)40', '(NULL)41', '(NULL)42', 
						'(NULL)43', '(NULL)44', '(NULL)45', '(NULL)46', '(NULL)47', '(NULL)48', '(NULL)49', '(NULL)50', -- 'ADMIN'
						 Usuario
				FROM (
						
						SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)),
									`Cuenta_T`, `Documento_T`, `Fecha_T`, `Creditos_N`, `ValorConsig_N`, 
									`Concod_N`, `DocumentoPago_T`, `FechaPago_T`, `Debitos_N`	
						FROM (
								(							
								SELECT 	'No_N' `No_N`,'Cuenta_T' `Cuenta_T`,'Documento_T' `Documento_T`,'Fecha_T' `Fecha_T`,
								'Creditos_N' `Creditos_N`,'ValorConsig_N' `ValorConsig_N`,'Concod_N' `Concod_N`,'DocumentoPago_T' `DocumentoPago_T`,
								'FechaPago_T' `FechaPago_T`,'Debitos_N' `Debitos_N`
								)
								UNION ALL
								(
								/*CRÉDITOS DESEMBOLSADOS antes de la fecha de corte*/
								SELECT @ID := IF((SELECT MAX(TASec) FROM temporalarchivos) IS NULL, 0, (SELECT MAX(TASec) FROM temporalarchivos)) `No_N`,
								Cuenta, Documento, Fecha, Creditos, ValorConsig, Concod,DocumentoPago,FechaPago,Debitos
							from
							(Select c5.cuenta Cuenta,c5.documento Documento,c5.fecha Fecha,c5.creditos creditos,c5.valor ValorConsig,c5.concod Concod ,ifnull(c6.transacciondoccon,0) DocumentoPago,ifnull(c6.fecha,'') FechaPago,ifnull(c6.Debitos2,0) Debitos
							from 
							(select C1.*,ifnull(c2.concod,0) concod,ifnull(c2.valor,0) valor
							from
							(select cuentaid cuenta, t.transaccionid transaccion, transacciondoccon documento,TransaccionFechaDoc fecha, sum(TransaccionCR) creditos 
								from mantiscoomuldenorteniif.transacciones t , mantiscoomuldenorteniif.transacciones1 t1
								where  t.transaccionid=t1.transaccionid and cuentaid in ('24459501','24451003001' )
								and TransaccionFechaDoc between FechaInicio and FechaFinal  
								group by t1.TransaccionId having creditos <> 0 order by t.transaccionid) c1
							Left join
							(Select transaccionid , ConCod, ConVal Valor from mantiscoomuldenorteniif.consignaciones t where conval<>0 order by transaccionid) c2
							on transaccion=transaccionid   order by concod  )c5
							left join
							 -- 2 cruce transacciones con consignacionesdet
							(Select c3.*,ifnull(c4.concod,0) concod,ifnull(c4.transaccionC,0) debitos,ifnull(valor,0) valorconsig
							from 
							(select t.TransaccionId,transacciondoccon,TransaccionFechaDoc fecha , sum(Transacciondb) Debitos2
								from mantiscoomuldenorteniif.transacciones t , mantiscoomuldenorteniif.transacciones1 t1
								where  t.transaccionid=t1.transaccionid and cuentaid in('24459501','24451003001' )
								and TransaccionFechaDoc between FechaInicio and FechaFinal 
								group by t1.TransaccionId having Debitos2 <> 0 order by t.transaccionid) c3
							left join
							(select concod,ConDetTranId transaccionC, ConDetVal valor 
							from  mantiscoomuldenorteniif.consignacionesdet cd order by concod)c4
							on c3.TransaccionId=c4.transaccionC) c6
							on c5.concod=c6.concod 
							where c6.concod<>0
							union ALL
							-- ********Registro en contabilidad que no estan en consignaciones***************
							SELECT cuentaid,TransaccionDocCon,TransaccionFechaDoc FECHA,(TransaccionCR) creditos,conval,concod,'NOREGISTRA PAGO','1000-01-01',0
							 FROM mantiscoomuldenorteniif.transacciones t , mantiscoomuldenorteniif.transacciones1 t1,mantiscoomuldenorteniif.consignaciones c
							 where  t.transaccionid=t1.transaccionid and cuentaid in ('24459501','24451003001' ) and c.transaccionid=t.transaccionid
							 and TransaccionFechaDoc between FechaInicio and FechaFinal 
                             and concod not in (select concod from mantiscoomuldenorteniif.consignacionesdet)
							 group by t.TransaccionId
							 having creditos>0) c7g
                             

                                 )							
							) T1
					) T1;			
END IF;

	IF NombreReporte = 'RPTDATACRE' THEN -- REPORTE DATACREDITO
			DELETE FROM TMPRPTDatacredito WHERE RPTCOLUSUCOD = Usuario;
			
			-- CARTERA DEUDORES VIGENTES
			DROP TABLE IF EXISTS VIGENTES;
			CREATE TEMPORARY TABLE VIGENTES
			SELECT TipDocDC RPTCOL1, AsoIde RPTCOL2, CieCarCarCod RPTCOL3,
				TRIM(CONCAT(IF(ASOPRIAPE IS NULL, ' ', ASOPRIAPE), ' ', IF(ASOSEGAPE IS NULL, ' ', ASOSEGAPE),' ',IF(ASOPRINOM IS NULL, '', ASOPRINOM), ' ', IF(ASOSEGNOM IS NULL, '',ASOSEGNOM)) ) RPTCOL4,
				0 RPTCOL5, FecDesCre RPTCOL6, FecVenCre RPTCOL7, "00" RPTCOL8, "2" RPTCOL9, 0 RPTCOL10,
				1 RPTCOL11, 0 RPTCOL12, 1 RPTCOL13, CieCarDiaMor RPTCOL14,
				CASE WHEN CARCODREE = '' THEN 0 ELSE 1 END RPTCOL15,
				FecDesCre RPTCOL16, CASE WHEN CieCarDiaMor = 0 THEN "01" ELSE "02" END RPTCOL17, FECHACORTE RPTCOL18, 0 RPTCOL19,
				"00000000" RPTCOL20, 0 RPTCOL21, "00000000" RPTCOL22, 0 RPTCOL23, 0 RPTCOL24, "" RPTCOL25,
				1 RPTCOL26, 0 RPTCOL27, CieCarEXCAR RPTCOL28, "000" RPTCOL29, CieCarDiaMor RPTCOL30,
				ROUND(SALINICRE/1000, 0) RPTCOL31, ROUND(CieCarSalAct/1000, 0) RPTCOL32, "00000000000" RPTCOL33,
				ROUND(ValCuo/1000, 0) RPTCOL34, 0 RPTCOL35, SOLCREPLA RPTCOL36, CieCarAltCre RPTCOL37,
				CieCarNCMor RPTCOL38, "000" RPTCOL39, "00000000" RPTCOL40, "1000-01-01" RPTCOL41,
				"0000000000" RPTCOL42, SucNom RPTCOL43, SucNom RPTCOL44, MunCod RPTCOL45,
				'' RPTCOL46, AsoLugRes RPTCOL47, "" RPTCOL48, AsoDir RPTCOL49, AsoCel RPTCOL50,
				"" RPTCOL51, AsoDLMun RPTCOL52, "" RPTCOL53, AsoDLDir RPTCOL54, AsoDLTel1 RPTCOL55,
				"" RPTCOL56, AsoLugRes RPTCOL57, "" RPTCOL58,
				AsoDir RPTCOL59, AsoEmail RPTCOL60, AsoCel RPTCOL61, CODIGO_SUSCRIPTOR RPTCOL62,
				"000000000000000000" RPTCOL63, CarPagVarTga RPTCOL64, "" RPTCOL65, "00000000" RPTCOL66,
				-- CAMPOS PARA CONEXION DE CONSULTAS
				1 INDICADOR, CieCarAltCre AltCre
			FROM ciecar C
			INNER JOIN asociados 				A ON C.CieCarAsoCod = A.AsoCod
            INNER JOIN tiposdedocumento 	T ON A.AsoTipIde = T.TipDocCod
			LEFT JOIN (SELECT MAX(ASODLCOD), DL.* FROM atlantiscoomuldenorteniif.asociadosDL DL GROUP BY ASOCOD) T1 ON A.ASOCOD = T1.ASOCOD
			INNER JOIN cartera 						C2 ON C.CieCarCarCod = C2.CarCod
			INNER JOIN SOLCRE 					S ON C2.SOLCRECOD = S.SOLCRECOD
			INNER JOIN SUCURSALES 		S2 ON S.SolCreSucCod = S2.SUCCOD			
            WHERE CieCarFecCie = FECHACORTE AND (FecCenRie = '1900-01-01' OR FecCenRie >= FECHACORTE);
            
            -- AJUSTAR ESTRUCTURA
			ALTER TABLE VIGENTES 
					MODIFY `RPTCOL46` VARCHAR(50) CHARACTER SET utf8  DEFAULT '' NULL,
					MODIFY `RPTCOL48` VARCHAR(50) CHARACTER SET utf8  DEFAULT '' NULL,
					MODIFY `RPTCOL53` VARCHAR(50) CHARACTER SET utf8  DEFAULT '' NULL,
					MODIFY `RPTCOL56` VARCHAR(50) CHARACTER SET utf8  DEFAULT '' NULL,
					MODIFY `RPTCOL58` VARCHAR(50) CHARACTER SET utf8  DEFAULT '' NULL;
				
			CREATE INDEX `UVIGENTES` ON `VIGENTES` (`RPTCOL3` ,`INDICADOR`);
            
			-- CREDITOS CANCELADOS
			INSERT INTO VIGENTES 
			SELECT * FROM (
						SELECT TipDocDC RPTCOL1, AsoIde RPTCOL2, CieCarCarCod RPTCOL3,
							TRIM(CONCAT(IF(ASOPRIAPE IS NULL, ' ', ASOPRIAPE), ' ', IF(ASOSEGAPE IS NULL, ' ', ASOSEGAPE),' ',IF(ASOPRINOM IS NULL, '', ASOPRINOM), ' ', IF(ASOSEGNOM IS NULL, '',ASOSEGNOM)) ) RPTCOL4,
							0 RPTCOL5, FecDesCre RPTCOL6, FecVenCre RPTCOL7, "00" RPTCOL8, "2" RPTCOL9, 0 RPTCOL10,
							1 RPTCOL11, 0 RPTCOL12, 1 RPTCOL13, '05' RPTCOL14, 0 RPTCOL15,
							FecDesCre RPTCOL16, "03" RPTCOL17, FECHACORTE RPTCOL18, 0 RPTCOL19,
							"00000000" RPTCOL20, 0 RPTCOL21, "00000000" RPTCOL22, 0 RPTCOL23, 0 RPTCOL24, "" RPTCOL25,
							1 RPTCOL26, 0 RPTCOL27, CieCarEXCAR RPTCOL28, "000" RPTCOL29, CieCarDiaMor RPTCOL30,
							ROUND(SALINICRE/1000, 0) RPTCOL31, 0 RPTCOL32, "00000000000" RPTCOL33,
							0 RPTCOL34, 0 RPTCOL35, SOLCREPLA RPTCOL36, SOLCREPLA RPTCOL37,
							0 RPTCOL38, "000" RPTCOL39, "00000000" RPTCOL40, "1000-01-01" RPTCOL41,
							"0000000000" RPTCOL42, SucNom RPTCOL43, SucNom RPTCOL44, MunCod RPTCOL45,
							'' RPTCOL46, AsoLugRes RPTCOL47, "" RPTCOL48, AsoDir RPTCOL49, AsoCel RPTCOL50,
							"" RPTCOL51, AsoDLMun RPTCOL52, "" RPTCOL53, AsoDLDir RPTCOL54, AsoDLTel1 RPTCOL55,
							"" RPTCOL56, AsoLugRes RPTCOL57, "" RPTCOL58,
							AsoDir RPTCOL59, AsoEmail RPTCOL60, AsoCel RPTCOL61, CODIGO_SUSCRIPTOR RPTCOL62,
							"000000000000000000" RPTCOL63, CarPagVarTga RPTCOL64, "" RPTCOL65, "00000000" RPTCOL66,
							-- CAMPOS PARA CONEXION DE CONSULTAS
							2 INDICADOR, CieCarAltCre AltCre
						FROM ciecar C
						INNER JOIN asociados 				A ON C.CieCarAsoCod = A.AsoCod
                        INNER JOIN tiposdedocumento 	T ON A.AsoTipIde = T.TipDocCod
						LEFT JOIN (SELECT MAX(ASODLCOD), DL.* FROM atlantiscoomuldenorteniif.asociadosDL DL GROUP BY ASOCOD) T1 ON A.ASOCOD = T1.ASOCOD
						INNER JOIN cartera 						C2 ON C.CieCarCarCod = C2.CarCod
						INNER JOIN SOLCRE 					S ON C2.SOLCRECOD = S.SOLCRECOD
						INNER JOIN SUCURSALES 		S2 ON S.SolCreSucCod = S2.SUCCOD
						WHERE CieCarFecCie = FECHA_ANTERIOR AND CIECARCARCOD NOT IN (SELECT CIECARCARCOD FROM CIECAR WHERE CieCarFecCie = FECHACORTE)
					) C;

			-- ACTIVOS CASTIGADOS
			INSERT INTO VIGENTES 
			SELECT * FROM (
						SELECT TipDocDC RPTCOL1, AsoIde RPTCOL2, ActCasCarCod RPTCOL3,
							TRIM(CONCAT(IF(ASOPRIAPE IS NULL, ' ', ASOPRIAPE), ' ', IF(ASOSEGAPE IS NULL, ' ', ASOSEGAPE),' ',IF(ASOPRINOM IS NULL, '', ASOPRINOM), ' ', IF(ASOSEGNOM IS NULL, '',ASOSEGNOM)) ) RPTCOL4,
							0 RPTCOL5, IFNULL(FecDesCre,  CONCAT(SUBSTRING(ActCasIniCre, 1, 4), '-', SUBSTRING(ActCasIniCre, 5, 2), '-', SUBSTRING(ActCasIniCre, 7, 2))) RPTCOL6,
							IFNULL(FecVenCre,  CONCAT(SUBSTRING(ActCasFinCre, 1, 4), '-', SUBSTRING(ActCasFinCre, 5, 2), '-', SUBSTRING(ActCasFinCre, 7, 2))) RPTCOL7,
							"00" RPTCOL8, "2" RPTCOL9, 0 RPTCOL10, 1 RPTCOL11, 0 RPTCOL12, 1 RPTCOL13, '13' RPTCOL14, 0 RPTCOL15,
							IFNULL(FecDesCre,  CONCAT(SUBSTRING(ActCasIniCre, 1, 4), '-', SUBSTRING(ActCasIniCre, 5, 2), '-', SUBSTRING(ActCasIniCre, 7, 2))) RPTCOL16,
							'06' RPTCOL17, '1000-01-01' RPTCOL18, 0 RPTCOL19, "00000000" RPTCOL20, 0 RPTCOL21, "00000000" RPTCOL22, 0 RPTCOL23, 0 RPTCOL24,
							"" RPTCOL25, 1 RPTCOL26, 0 RPTCOL27, ActCasCat RPTCOL28, "000" RPTCOL29, ActCasDiaMor RPTCOL30, ROUND(ActCasSalCap/1000, 0) RPTCOL31,
							0 RPTCOL32, "00000000000" RPTCOL33, 0 RPTCOL34, 0 RPTCOL35, 1 RPTCOL36, 0 RPTCOL37, 0 RPTCOL38, "000" RPTCOL39, "00000000" RPTCOL40,
							ActCasFecCas RPTCOL41, ActCasFecCas RPTCOL42, IFNULL(SucNom, 'CUCUTA') RPTCOL43, IFNULL(SucNom, 'CUCUTA') RPTCOL44,
							IFNULL(MunCod, '54001') RPTCOL45, '' RPTCOL46, AsoLugRes RPTCOL47, "" RPTCOL48, AsoDir RPTCOL49, AsoCel RPTCOL50,
							"" RPTCOL51, AsoDLMun RPTCOL52, "" RPTCOL53, AsoDLDir RPTCOL54, AsoDLTel1 RPTCOL55, "" RPTCOL56, AsoLugRes RPTCOL57,
							"" RPTCOL58, AsoDir RPTCOL59, AsoEmail RPTCOL60, AsoCel RPTCOL61, CODIGO_SUSCRIPTOR RPTCOL62,
							"000000000000000000" RPTCOL63, IFNULL(CarPagVarTga, 2) RPTCOL64, "" RPTCOL65, "00000000" RPTCOL66,
							-- CAMPOS PARA CONEXION DE CONSULTAS
							3 INDICADOR, 1 AltCre
						FROM ACTCAS T1
						INNER JOIN asociados 				T2 ON T1.ActCasAsoCod = T2.AsoCod
                        INNER JOIN tiposdedocumento 	T ON T2.AsoTipIde = T.TipDocCod
						LEFT JOIN (SELECT MAX(ASODLCOD), DL.* FROM atlantiscoomuldenorteniif.asociadosDL DL GROUP BY ASOCOD) T4 ON T2.ASOCOD = T4.ASOCOD
						LEFT JOIN cartera 						T3 ON T1.ActCasCarCod = T3.CarCod
						LEFT JOIN SOLCRE 					S ON T3.SOLCRECOD = S.SOLCRECOD
						LEFT JOIN SUCURSALES 			S2 ON S.SolCreSucCod = S2.SUCCOD
						WHERE ActCasFecCas >= FECHA_INICIO AND ActCasFecCas <= FECHACORTE AND ActCasMod = 'CA' AND ActCasTipCas <> 'E' AND (ActcasFecCenRie = '1900-01-01'  OR ActcasFecCenRie >= FECHACORTE)

					) C2;

			-- CONSULTA LOS CODEUDORES
			DROP TABLE IF EXISTS CODEUDORES;
			CREATE TEMPORARY TABLE CODEUDORES
			SELECT TipDocDC, T.SolCreCODIde, T1.CarCod,
				CASE WHEN T.SolCreCODTip = 'A'
					THEN TRIM(CONCAT(IF(ASOPRIAPE IS NULL, ' ', ASOPRIAPE), ' ', IF(ASOSEGAPE IS NULL, ' ', ASOSEGAPE),' ',IF(ASOPRINOM IS NULL, '', ASOPRINOM), ' ', IF(ASOSEGNOM IS NULL, '',ASOSEGNOM)) )
					ELSE TRIM(CONCAT(IF(CodPriApe IS NULL, ' ', CodPriApe), ' ', IF(CodSegApe IS NULL, ' ', CodSegApe),' ',IF(CodPriNom IS NULL, '', CodPriNom), ' ', IF(CodSegNom IS NULL, '',CodSegNom)) ) END NOMBRE,
					RPTCOL5, RPTCOL6, RPTCOL7, '01' RPTCOL8, RPTCOL9, RPTCOL10, RPTCOL11, RPTCOL12, RPTCOL13, RPTCOL14,
					RPTCOL15, RPTCOL16, RPTCOL17, RPTCOL18, RPTCOL19, RPTCOL20, RPTCOL21, RPTCOL22, RPTCOL23,
					RPTCOL24, RPTCOL25, RPTCOL26, RPTCOL27, RPTCOL28, RPTCOL29, RPTCOL30, RPTCOL31, RPTCOL32,
					RPTCOL33, RPTCOL34, RPTCOL35, RPTCOL36, RPTCOL37, RPTCOL38, RPTCOL39, RPTCOL40, RPTCOL41,
					RPTCOL42, RPTCOL43, RPTCOL44, RPTCOL45, RPTCOL46,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoLugRes ELSE CodLugRes END RPTCOL47, RPTCOL48,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoDir ELSE CodDir END RPTCOL49,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoCel ELSE CodCel END RPTCOL50,
					RPTCOL51, CASE WHEN T.SolCreCODTip = 'A' THEN AsoDLMun ELSE '' END RPTCOL52, RPTCOL53,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoDLDir ELSE '' END RPTCOL54,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoDLTel1 ELSE '' END RPTCOL55, RPTCOL56,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoLugRes ELSE CodLugRes END RPTCOL57, RPTCOL58,
					CASE WHEN T.SolCreCODTip = 'A' THEN AsoDir ELSE CodDir END RPTCOL59,
					RPTCOL60, CASE WHEN T.SolCreCODTip = 'A' THEN AsoDLTel1 ELSE CodCel END RPTCOL61,
					RPTCOL62, RPTCOL63, RPTCOL64, RPTCOL65, RPTCOL66, 4 INDICADOR, T4.ALTCRE
			FROM SOLCRECOD T
			INNER JOIN tiposdedocumento 	T5 ON T.SolCreCODTipIde = T5.TipDocCod
			INNER JOIN CARTERA 				T1 ON T.SOLCRECOD = T1.SOLCRECOD
			LEFT JOIN CODEUDOR 				T2 ON T.SolCreCODIde = T2.CodIde AND SolCreCODTip = 'C'
			LEFT JOIN ASOCIADOS 				T3 ON T.SolCreCODIde = T3.ASOIDE AND SolCreCODTip = 'A'
			LEFT JOIN (SELECT MAX(ASODLCOD), DL.* FROM asociadosDL DL GROUP BY ASOCOD) T6 ON T3.ASOCOD = T6.ASOCOD AND SolCreCODTip = 'A'
			INNER JOIN VIGENTES 				T4 ON T1.CARCOD = T4.RPTCOL3;

			-- INSERTA CDEUDORES
			INSERT INTO VIGENTES
			SELECT * FROM CODEUDORES;
					
			/***** ACTUALIZACION DE DATOS *****/
			-- CREDITOS REESTRUCTURADOS POR ATLANTIS
			UPDATE VIGENTES V
			INNER JOIN (
					SELECT CARCODREE FROM CARTERA WHERE CARCODREE <> '' GROUP BY CARCODREE
				) T1 ON V.RPTCOL3 = T1.CARCODREE
			SET V.RPTCOL15 = 1
			WHERE V.INDICADOR <> 3 AND V.RPTCOL15 <> 2;

			-- CREDITOS REFINANCIADOS POR ATLANTIS
			UPDATE VIGENTES V
			INNER JOIN (
					SELECT CARCODREF FROM CARTERA WHERE CARCODREF <> '' GROUP BY CARCODREF
				) T1 ON V.RPTCOL3 = T1.CARCODREF
			SET V.RPTCOL15 = 2
			WHERE V.INDICADOR <> 3 AND V.RPTCOL15 <> 2;

			-- PROXIMA FECHA DE PAGO
			UPDATE VIGENTES V
			INNER JOIN TABAMOCRE T ON V.RPTCOL3 = T.CARCOD
			SET RPTCOL41 = T.TabAmoFPa
			WHERE TabAmoNCu = V.ALTCRE+1 AND INDICADOR IN(1,4);

			-- ULTIMA FECHA DE PAGO
			UPDATE VIGENTES V
			INNER JOIN (
					SELECT CarCod, MAX(aFecReaPag) FECHA
					FROM pagoscarteraa
					WHERE aFecReaPag <= FECHACORTE
					GROUP BY CARCOD
				) T1 ON V.RPTCOL3 = T1.CarCod
			SET RPTCOL42 = FECHA;

			-- CIUDAD DE LA OBLIGACION
			UPDATE VIGENTES V
			INNER JOIN MUNICIPIOS M ON M.MUNCOD = V.RPTCOL45
			SET V.RPTCOL44 = M.MunNombre;

			-- CIUDAD DE RESIDENCIA
			UPDATE VIGENTES V
			INNER JOIN MUNICIPIOS M ON M.MUNCOD = V.RPTCOL47
			SET V.RPTCOL46 = M.MunNombre;

			-- DEPARTAMENTO DE RESIDENCIA
			UPDATE VIGENTES V
			INNER JOIN MUNICIPIOS M ON M.MUNCOD = V.RPTCOL47
			INNER JOIN DEPARTAMENTOS D ON M.DEPCOD = D.DEPCOD
			SET V.RPTCOL46 = M.MunNombre, V.RPTCOL48 = DepNombre, V.RPTCOL56 = M.MunNombre, V.RPTCOL58 = DepNombre;

			-- DEPARTAMENTO LABORAL
			UPDATE VIGENTES V
			INNER JOIN MUNICIPIOS M ON M.MUNCOD = V.RPTCOL52
			INNER JOIN DEPARTAMENTOS D ON M.DEPCOD = D.DEPCOD
			SET V.RPTCOL53 = DepNombre;

			-- FORMA DE PAGO PARA CREDITOS CANCELADOS VOLUNTARIAMENTE
			UPDATE VIGENTES V
			INNER JOIN (
						SELECT CarCod COD, MAX(aFecReaPag) FECHA, aNumCuoPK, 1 FORMAPAGO
						FROM pagoscarteraa
						WHERE aFecReaPag <= FECHACORTE
						GROUP BY CARCOD
					) T ON V.RPTCOL3 = T.COD
			SET V.RPTCOL12 = T.FORMAPAGO
			WHERE INDICADOR = 2;

			-- FORMA DE PAGO Y ESTADO DE LA CUENTA PARA CREDITOS REESTRUCTURADOS O NOVADOS
			UPDATE VIGENTES V
			INNER JOIN CARTERA C ON V.RPTCOL3 = C.CARCOD
			SET RPTCOL12 = '4', RPTCOL17 = "12"
			WHERE INDICADOR = 2 AND RPTCOL12 = 1 AND (CARCODREE <> '' OR CARCODREF <> '');

			-- ACTUALIZA CREDITOS CASTIGADOS: FORMA DE PAGO - NOVEDAD - FECHA DE LA CUENTA
			UPDATE VIGENTES V
			INNER JOIN (
					SELECT ActCasCarCod COD, ActCasValCap + ActCasValInt - SUM(ActCasDetValPag) SALDO, MAX(ActCasDetFec) FECHA
					FROM ACTCAS A
					INNER JOIN actcasdet D ON A.ActCasCod = D.ActCasCod
					WHERE ActcasFecCenRie <> '1753-01-01'
					GROUP BY A.ActCasCod
					HAVING SALDO <= 0
				) T ON V.RPTCOL3 = T.COD
			-- PAGO VOLUNTARIO - CARTERA RECUPERADA - FECHA DE ULTIMO ABONO
			SET RPTCOL12 = 1, RPTCOL14 = '14', RPTCOL18 = FECHA
			WHERE INDICADOR = 3;

			-- ESTADO DE LA CUENTA PARA CREDITOS CANCELADOS POR ABONO NORMAL O AVANCE A CAPITAL
			UPDATE VIGENTES V
			INNER JOIN (
						SELECT CarCod COD, MAX(aFecReaPag) FECHA, aNumCuoPK, CASE WHEN aNumCuoPK = 0 THEN "03" ELSE "08" END FORMAPAGO
						FROM pagoscarteraa
						WHERE aFecReaPag <= FECHACORTE
						GROUP BY CARCOD
					) T ON V.RPTCOL3 = T.COD
			SET V.RPTCOL17 = T.FORMAPAGO
			WHERE INDICADOR = 2 AND RPTCOL17 <> "12";

			-- FECHA ESTADO DE LA CUENTA PARA CREDITOS CANCELADOS
			UPDATE VIGENTES
			SET RPTCOL18 = RPTCOL42
			WHERE INDICADOR = 2;

			-- FECHA ESTADO DE LA CUENTA Y SALDO ACTUAL PARA CREDITOS CASTIGADOS
			UPDATE VIGENTES V
			INNER JOIN (
					SELECT ActCasCarCod COD, ROUND((ActCasValCap + ActCasValInt - SUM(ActCasDetValPag)) / 1000, 0) SALDO, MAX(ActCasDetFec) FECHA
					FROM ACTCAS A
					INNER JOIN actcasdet D ON A.ActCasCod = D.ActCasCod
					WHERE ActcasFecCenRie <> '1753-01-01'
					GROUP BY A.ActCasCod
					HAVING SALDO > 0
				) T ON V.RPTCOL3 = T.COD
			SET RPTCOL18 = FECHA, RPTCOL32 = SALDO, RPTCOL34 = SALDO, RPTCOL35 = SALDO;

			-- ACTUALIZAR TIPO DE OBLIGACION PARA CREDITOS DE VIVIENDA
			UPDATE Vigentes T
			INNER JOIN (
					-- DETERMINA CUALES CREDITOS SON CREDITO DE VIVIENDA
					SELECT CARCOD, T3.SolCreCod, TipCreCod
					FROM  Cartera T  
					INNER JOIN SolCre T3 ON T.SolCreCod = T3.SolCreCod
					WHERE TipCreCod = 3
			)T2 ON T.RPTCOL3 = T2.CARCOD
			SET T.RPTCOL9 = '3'
			WHERE INDICADOR <> 3;

			-- ACTUALIZAR TIPO DE OBLIGACION PARA CREDITO COMERCIAL Y FORMA DE PAGO CAJA
			UPDATE VIGENTES T1
			INNER JOIN (
					SELECT  T3.CARCOD CARCOD,T1.TipCreDes,T2.TipCreVarFPa  FROM SOLCRE  T
					INNER JOIN tipocreditos T1 ON T.TipCreCod =T1.TipCreCod 
					INNER JOIN tipocreditosvariable T2 ON T.TipCreCod =T2.TipCreCod AND T.TipCreVarCod = T2.TipCreVarCod
					INNER JOIN CARTERA T3 ON T.SOLCRECOD = T3.SOLCRECOD 
					WHERE T.TipCreCod = 4 AND TipCreVarFPa = 2
			) T2 ON T1.RPTCOL3 = T2.CARCOD
			SET T1.RPTCOL9 = '1'
			WHERE INDICADOR <> 3;

			-- ACTUALIZAR TIPO DE OBLIGACION PARA CREDITO CONSUMO Y FORMA DE PAGO CAJA
			UPDATE VIGENTES T1
			INNER JOIN (
					SELECT  T3.CARCOD CARCOD,T1.TipCreDes,T2.TipCreVarFPa  FROM SOLCRE  T
					INNER JOIN tipocreditos T1 ON T.TipCreCod =T1.TipCreCod 
					INNER JOIN tipocreditosvariable T2 ON T.TipCreCod =T2.TipCreCod AND T.TipCreVarCod = T2.TipCreVarCod
					INNER JOIN CARTERA T3 ON T.SOLCRECOD = T3.SOLCRECOD 
					WHERE T.TipCreCod <> 3 AND TipCreVarFPa = 1
			) T2 ON T1.RPTCOL3 = T2.CARCOD
			SET T1.RPTCOL9 = '6'
			WHERE INDICADOR <> 3;

			-- ACTUALIZA EL VALOR SALDO EN MORA
			UPDATE VIGENTES T1
			INNER JOIN (
						SELECT T.COD COD, ROUND((T.SALDO_MORA - T1.ABONO_INTERES) / 1000, 0) SALDO_MORA
						FROM (
									SELECT  T.CarCod COD, ROUND(SUM(TabAmoVCa) + SUM(TabAmoVIn) + SUM(TabAmoVSe) + SUM(TabAmoVOt), 0) SALDO_MORA
									FROM tabamocre T
									INNER JOIN CIECAR C ON T.CARCOD = C.CIECARCARCOD
									WHERE CIECARFECCIE = FECHACORTE AND TabAmoNCu > CieCarAltCre AND TabAmoFPa < FECHACORTE
									GROUP BY T.CarCod
							) T
						INNER JOIN (
									SELECT  T.CarCod COD, ROUND(SUM(aValDisInt) , 0) ABONO_INTERES
									FROM PAGOSCARTERAA T
									INNER JOIN CIECAR C ON T.CARCOD = C.CIECARCARCOD
									WHERE CIECARFECCIE = FECHACORTE AND aNumCuoPag > CieCarAltCre AND aFecReaPag <= FECHACORTE
									GROUP BY T.CarCod
							) T1 ON T.COD = T1.COD
						WHERE T.SALDO_MORA > 0
				) T2 ON T1.RPTCOL3 = T2.COD
			SET T1.RPTCOL35 = T2.SALDO_MORA
            WHERE T2.SALDO_MORA > 0;
			
            -- ACTUALIZA EL SALDO ACTUAL SEGUN PARAMETRO DE MINIMO A REPORTAR (1000), NOVEDAD Y FORMA DE PAGO
			UPDATE VIGENTES
			SET RPTCOL12 = 1, RPTCOL32 = 0,  RPTCOL14 = '05'
			WHERE RPTCOL32 <= 1 AND INDICADOR IN(1, 4);

			-- ACTUALIZA CREDITOS MORA < 10 MIL Y SALDO > MIL, FORMA DE PAGO, SALDO EN MORA, EDAD Y CALIFICACION
			UPDATE VIGENTES
			SET RPTCOL35 = 0, RPTCOL30 = 0, RPTCOL28 = 'A',  RPTCOL12 = 0, RPTCOL14 = '01'
			WHERE RPTCOL28 <> 'A' AND RPTCOL35 <= 10 AND RPTCOL32 > 1;
			
            -- ACTUALIZA LA NOVEDAD PARA LOS CREDITOS QUE ESTAN EN CATEGORIA A Y SIN SALDO DE MORA
			UPDATE VIGENTES
			SET RPTCOL14 = '01'
			WHERE RPTCOL28 = 'A' AND RPTCOL30 = 0;
		
			-- ACTUALIZA VALOR EN MORA PARA CREDITOS EN CATEGORIA A
			UPDATE VIGENTES
			SET RPTCOL35 = 0, RPTCOL38 = 0
			WHERE RPTCOL28 = 'A';
            
			-- RESULTADO FINAL
			-- SELECT * FROM VIGENTES;

			-- INSERTA REGISTRO EN TABLA GX
			INSERT INTO TMPRPTDatacredito
			(
				RPTCOL1, RPTCOL2, RPTCOL3, RPTCOL4, RPTCOL5, RPTCOL6, RPTCOL7, RPTCOL8, RPTCOL9,
				RPTCOL10, RPTCOL11, RPTCOL12, RPTCOL13, RPTCOL14, RPTCOL15, RPTCOL16, RPTCOL17, RPTCOL18,
				RPTCOL19, RPTCOL20, RPTCOL21, RPTCOL22, RPTCOL23, RPTCOL24, RPTCOL25, RPTCOL26, RPTCOL27,
				RPTCOL28, RPTCOL29, RPTCOL30, RPTCOL31, RPTCOL32, RPTCOL33, RPTCOL34, RPTCOL35, RPTCOL36,
				RPTCOL37, RPTCOL38, RPTCOL39, RPTCOL40, RPTCOL41, RPTCOL42, RPTCOL43, RPTCOL44, RPTCOL45,
				RPTCOL46, RPTCOL47, RPTCOL48, RPTCOL49, RPTCOL50, RPTCOL51, RPTCOL52, RPTCOL53, RPTCOL54,
				RPTCOL55, RPTCOL56, RPTCOL57, RPTCOL58, RPTCOL59, RPTCOL60, RPTCOL61, RPTCOL62, RPTCOL63,
				RPTCOL64, RPTCOL65, RPTCOL66, RPTCOL67, RPTCOL68, RPTCOL69, RPTCOLUSUCOD, RPTCOLINDICADOR
			)
			SELECT RPTCOL1, RPTCOL2, RPTCOL3, RPTCOL4, RPTCOL5, RPTCOL6, RPTCOL7, RPTCOL8, RPTCOL9,
				RPTCOL10, RPTCOL11, RPTCOL12, RPTCOL13, RPTCOL14, RPTCOL15, RPTCOL16, RPTCOL17, RPTCOL18,
				RPTCOL19, RPTCOL20, RPTCOL21, RPTCOL22, RPTCOL23, RPTCOL24, RPTCOL25, RPTCOL26, RPTCOL27,
				RPTCOL28, RPTCOL29, RPTCOL30, RPTCOL31, RPTCOL32, RPTCOL33, RPTCOL34, RPTCOL35, RPTCOL36,
				RPTCOL37, RPTCOL38, RPTCOL39, RPTCOL40, RPTCOL41, RPTCOL42, RPTCOL43, RPTCOL44, RPTCOL45,
				RPTCOL46, RPTCOL47, RPTCOL48, RPTCOL49, RPTCOL50, RPTCOL51, RPTCOL52, RPTCOL53, RPTCOL54,
				RPTCOL55, RPTCOL56, RPTCOL57, RPTCOL58, RPTCOL59, RPTCOL60, RPTCOL61, RPTCOL62, RPTCOL63,
				RPTCOL64, RPTCOL65, RPTCOL66, '', '', '', Usuario, INDICADOR
			FROM VIGENTES;
            
			-- ACTUALIZACION DE FECHA EN QUE SE REPORTA EL CREDITO COMO CANCELADO YA SEA POR PAGO TOTAL O SALDO MENOR A 1000
			-- CREDITOS VIGENTES CON SALDO < 1000 PARA NO REPORTARLOS DE NUEVO
			UPDATE CARTERA C
			INNER JOIN TMPRPTDATACREDITO D ON C.CARCOD = D.RPTCOL3
			SET FecCenRie = FECHACORTE
			WHERE RPTCOLINDICADOR = 1 AND RPTCOL32 <= 1 AND FecCenRie = '1900-01-01';

			-- CREDITOS CANCELADOS DURANTE EL PERIODO
			UPDATE CARTERA C
			INNER JOIN TMPRPTDATACREDITO D ON C.CARCOD=D.RPTCOL3
			SET FecCenRie = FECHACORTE
			WHERE RPTCOLINDICADOR = 2 AND FECCENRIE = '1900-01-01';

			-- CASTIGOS RECUPERADOS EN EL PERIODO
			UPDATE ACTCAS A
			INNER JOIN TMPRPTDATACREDITO D ON A.ActCasCarCod = D.RPTCOL3
			SET ACTCASFECCENRIE = FECHACORTE
			WHERE RPTCOLINDICADOR = 3 AND RPTCOL32 = 0 AND ACTCASFECCENRIE = '1900-01-01';

	END IF;
END