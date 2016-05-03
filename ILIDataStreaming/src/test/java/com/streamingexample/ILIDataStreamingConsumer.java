package com.streamingexample;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import kafka.consumer.Consumer;
import kafka.consumer.ConsumerConfig;
import kafka.consumer.ConsumerIterator;
import kafka.consumer.KafkaStream;
import kafka.javaapi.consumer.ConsumerConnector;
import java.sql.*;

public final class ILIDataStreamingConsumer {
	
public static void main(String[] args) throws ClassNotFoundException, SQLException, InterruptedException {
	
		Connection conn = null;
		Statement stmt = null;
		String data = null;
		String[] iliDetails = new String[12];
		
		String group = "groupone" ;
		Properties props = new Properties();
        props.put("zookeeper.connect", "localhost:2181");
        props.put("group.id", group);
        props.put("zookeeper.session.timeout.ms", "413");
        props.put("zookeeper.sync.time.ms", "203");
        props.put("auto.commit.interval.ms", "1000");
        
        ConsumerConfig cf = new ConsumerConfig(props) ;
        ConsumerConnector consumer = Consumer.createJavaConsumerConnector(cf) ;
        String topic = "mytopic" ;
        
        Map<String, Integer> topicCountMap = new HashMap<String, Integer>();
        topicCountMap.put(topic, new Integer(1));
        Map<String, List<KafkaStream<byte[], byte[]>>> consumerMap = consumer.createMessageStreams(topicCountMap);
        List<KafkaStream<byte[], byte[]>> streams = consumerMap.get(topic);
	    KafkaStream<byte[],byte[]> stream = streams.get(0) ;
	    
	    Class.forName("com.mysql.jdbc.Driver");
	    conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/ili_prediction","root","");
	    stmt = conn.createStatement();
	    String sql;
	    
	    ConsumerIterator<byte[], byte[]> it = stream.iterator();
        while (it.hasNext()) {
        	//Thread.sleep(1000);
            data = new String(it.next().message());
            System.out.println(data);
            String[] tempData = data.split(",");
            System.out.println("ili length" + iliDetails.length);
            
            for(int i=0; i<tempData.length;i++){
            	iliDetails[i]= tempData[i];
            }

            if(iliDetails[10]==null)
            	iliDetails[10]="";
            
            if(tempData.length == 12)
            	sql = "INSERT into finaldataset VALUES ('"+iliDetails[0]+"','"+iliDetails[1]+"','"+iliDetails[2]+"','"+iliDetails[3]+"','"+iliDetails[4]+"','"+iliDetails[5]+"','"+iliDetails[6]+"','"+iliDetails[7]+"','"+iliDetails[8]+"','"+iliDetails[9]+"','"+iliDetails[10]+"','"+iliDetails[11]+"')";
            else
            	sql = "INSERT into compute_target VALUES ('"+iliDetails[0]+"','"+iliDetails[1]+"','"+iliDetails[2]+"','"+iliDetails[3]+"','"+iliDetails[4]+"','"+iliDetails[5]+"','"+iliDetails[6]+"','"+iliDetails[7]+"','"+iliDetails[8]+"','"+iliDetails[9]+"','"+iliDetails[10]+"')";
            
           stmt.executeUpdate(sql);
        }
        consumer.shutdown(); 
	}
	
	

}
