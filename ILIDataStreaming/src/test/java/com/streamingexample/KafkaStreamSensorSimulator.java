package com.streamingexample;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Properties;

import kafka.javaapi.producer.Producer;
import kafka.producer.KeyedMessage;
import kafka.producer.ProducerConfig;

public class KafkaStreamSensorSimulator {

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

		String csvFile = "C:\\Users\\Vasu\\SJSU\\Spring2016\\239\\Project\\Flu-Prediction\\data\\FinalDataSet_3_3.csv";
		BufferedReader br = null;
		String line = "";

		try {

			br = new BufferedReader(new FileReader(csvFile));
			while ((line = br.readLine()) != null) {

				// use comma as separator
				// fluData = line.split(cvsSplitBy);

				System.out.println(" This is message " + line.toString());

				KeyedMessage<String, String> data = new KeyedMessage<String, String>(topic, String.valueOf(line),
						line.toString());
				Thread.sleep(2000);
				producer.send(data);

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

		System.out.println("Done");
		producer.close();

	}

}