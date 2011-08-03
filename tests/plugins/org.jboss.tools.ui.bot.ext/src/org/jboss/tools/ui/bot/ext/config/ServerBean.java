package org.jboss.tools.ui.bot.ext.config;

import java.io.File;

import org.jboss.tools.ui.bot.ext.helper.FileHelper;

/**
 * 
 * @author lzoubek
 *
 */
public class ServerBean extends RuntimeBean {

	public String withJavaVersion;
	public String type;
	/**
	 * name of remote system for this server, if null, server is local
	 */
	public String remoteSystem;
	/**
	 * home of app server located in remote system
	 */
	public String remoteHome;
	public static ServerBean fromString(String propValue, String url) throws Exception {
		ServerBean bean = fromString(propValue);
		if (bean!=null && url!=null) {
			String runtimeFile = downloadRuntime(url);
			if (runtimeFile!=null) {
				// where to unzip it?
				File runtimeOutput;
				File runtimeHomeAbs = new File(bean.runtimeHome).getAbsoluteFile();
				if (TestConfigurator.Values.SERVER_TYPE_JBOSSAS.equals(bean.type)) {
					runtimeOutput=runtimeHomeAbs.getParentFile();
				}
				else {
					runtimeOutput=runtimeHomeAbs.getParentFile().getParentFile();
				}
				FileHelper.unzipArchive(new File(runtimeFile), runtimeOutput);
			}			
		}
		return bean;
	}
	/**
	 * creates bean instance from property string
	 * @param propValue property value
	 * @return
	 * @throws Exception
	 */
	public static ServerBean fromString(String propValue) throws Exception {
		ServerBean bean = new ServerBean();
		try {
			if (propValue==null) {
				return null;
			}
		String[] serverParams = propValue.split(",");		
		bean.withJavaVersion = serverParams[2];		
		bean.runtimeHome=new File(serverParams[3]).getAbsolutePath();
		bean.version=serverParams[1];
		bean.type =  serverParams[0];
		
		if (serverParams.length>4) {
			bean.remoteSystem=serverParams[4];
			bean.remoteHome=serverParams[5];
		}
		return bean;
		}
		catch (Exception ex) {
			throw new Exception("Cannot parse "+bean.key+" property line",ex);
		}
	}
	@Override
	public String toString() {
		return String.format("%s type=%s,version=%s,home=%s,withJava=%s,remoteSystem=%s",this.key, this.type,this.version,this.runtimeHome,this.withJavaVersion,this.remoteSystem);
	}
	
	/**
	 * Returns server runtime name ([type]-[version])
	 * @return server runtime name
	 */
	public String getName() {
		return type + "-" + version; 
	}
	public ServerBean() {
		this.key = TestConfigurator.Keys.SERVER;
	}
	
}
