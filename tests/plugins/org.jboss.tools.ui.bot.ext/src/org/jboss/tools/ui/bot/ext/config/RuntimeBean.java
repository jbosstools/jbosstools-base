package org.jboss.tools.ui.bot.ext.config;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.net.HttpURLConnection;
import java.net.URL;

import org.apache.log4j.Logger;
import org.jboss.tools.ui.bot.ext.helper.FileHelper;

/**
 * Ancestor of all configurable beans, having common methods for parsing property line or downloading runtime
 * @author lzoubek
 *
 */
public class RuntimeBean {
	protected static final Logger log = Logger.getLogger(RuntimeBean.class);
	protected String key;
	public String version;
	public String runtimeHome;
	
	protected static RuntimeBean fromString(String propValue, RuntimeBean bean) throws Exception{
		try {
			if (propValue==null) {
				return null;
			}
			String[] params = propValue.split(",");			
			bean.runtimeHome=new File(params[1]).getAbsolutePath();
			bean.version=params[0];
			return bean;
			}
			catch (Exception ex) {
				throw new Exception("Cannot parse "+bean.key+" property line",ex);
			}
	}
	
	protected static RuntimeBean fromString(String propValue, String url, RuntimeBean bean) throws Exception {
		bean = fromString(propValue, bean);
		if (bean!=null && url!=null) {
			String runtimeFile = downloadRuntime(url);
			if (runtimeFile!=null) {
				File runtimeHomeAbs = new File(bean.runtimeHome).getAbsoluteFile();
				FileHelper.unzipArchive(new File(runtimeFile), runtimeHomeAbs.getParentFile());
			}			
		}
		return bean;
	}
	@Override
	public String toString() {
		return String.format("%s runtime version=%s, home=%s",this.key,
				this.version, this.runtimeHome);
	}

	/**
	 * Downloads file from given url to temp dir
	 * 
	 * @return absolute path to downloaded file
	 */
	protected static String downloadRuntime(String url) {
		BufferedInputStream in = null;
		RandomAccessFile raf = null;
		String outputFile = System.getProperty("java.io.tmpdir")+System.getProperty("file.separator")+url.replaceAll(".*/", "");
		log.info(String.format("Downloading %s to %s",url,outputFile));
		if (new File(outputFile).exists()) {
			log.info("Output file already exists, skipping");
			return outputFile;
		}
		int BUFFER_SIZE=8196;
		try {
			// open Http connection to URL
			HttpURLConnection conn = (HttpURLConnection)new URL(url).openConnection();
			// connect to server
			conn.connect();

			// Make sure the response code is in the 200 range.
			if (conn.getResponseCode() != 200) {
				log.error("Cannot download file, server returned "+conn.getResponseCode()+" : "+conn.getResponseMessage());
				return null;
			}

			// get the input stream
			in = new BufferedInputStream(conn.getInputStream());

			// open the output file and seek to the start location
			raf = new RandomAccessFile(outputFile, "rw");

			byte data[] = new byte[BUFFER_SIZE];
			int numRead;
			while(((numRead = in.read(data,0,BUFFER_SIZE)) != -1))
			{
				// write to buffer
				raf.write(data,0,numRead);
			}
			log.info("DONE");
			return outputFile;

		} catch (IOException e) {
			log.error(e);
			return null;
		} finally {
			if (raf != null) {
				try {
					raf.close();
				} catch (IOException e) {}
			}

			if (in != null) {
				try {
					in.close();
				} catch (IOException e) {}
			}
		}
	}

}
