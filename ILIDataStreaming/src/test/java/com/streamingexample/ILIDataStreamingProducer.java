package com.streamingexample;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

import kafka.javaapi.producer.Producer;
import kafka.producer.KeyedMessage;
import kafka.producer.ProducerConfig;

public class ILIDataStreamingProducer {

	public static void main(String[] args) throws InterruptedException {
		// TODO Auto-generated method stub
		Properties props = new Properties();

		props.put("metadata.broker.list", "localhost:9092");
		props.put("serializer.class", "kafka.serializer.StringEncoder");
		// props.put("partitioner.class", "example.producer.SimplePartitioner");
		props.put("request.required.acks", "1");

		ProducerConfig config = new ProducerConfig(props);

		Producer<String, String> producer = new Producer<String, String>(config);

		String topic = "mytopic";

		
		String csvFile1 = "\\Users\\kalpanatripathi\\Documents\\sjsu\\Flu-Prediction-master\\data\\compute_target.csv";
		String csvFile2 = "\\Users\\kalpanatripathi\\Documents\\sjsu\\Flu-Prediction-master\\data\\FinalDataSet_3_3.csv";
		
		streamData(csvFile1,producer, topic); 
		streamData(csvFile2,producer, topic);
		
		System.out.println("Data is done");
		producer.close();

	}
	
	public static void streamData(String csvFile, Producer<String, String> producer, String topic) throws InterruptedException{
		boolean isExcelColumn = false;
		BufferedReader br = null;
			String line = "";

		try {
			br = new BufferedReader(new FileReader(csvFile));
			while ((line = br.readLine()) != null) {
				System.out.println(line.toString());
				KeyedMessage<String, String> data = new KeyedMessage<String, String>(topic, String.valueOf(line),
						line.toString());
				if(isExcelColumn)
					producer.send(data);
				isExcelColumn = true;
				Thread.sleep(500);
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (br != null) {
				try {
					br.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

}