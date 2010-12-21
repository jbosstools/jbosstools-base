package org.jboss.tools.ui.bot.ext.config;

/**
 * 
 * @author lzoubek
 *
 */
public class ServerBean {

	public String version;
	public String runtimeHome;
	public String withJavaVersion;
	public String type;
	
	/**
	 * creates bean instance from property string
	 * @param propValue property value
	 * @return
	 * @throws Exception
	 */
	public static ServerBean fromString(String propValue) throws Exception {
		try {
			if (propValue==null) {
				return null;
			}
		String[] serverParams = propValue.split(",");
		ServerBean bean = new ServerBean();
		bean.withJavaVersion = serverParams[2];		
		bean.runtimeHome=serverParams[3];
		bean.version=serverParams[1];
		bean.type =  serverParams[0];
		return bean;
		}
		catch (Exception ex) {
			throw new Exception("Cannot parse SERVER property line",ex);
		}
	}
	@Override
	public String toString() {
		return String.format("Server type=%s,version=%s,home=%s,withJava=%s", this.type,this.version,this.runtimeHome,this.withJavaVersion);
	}
	
	/**
	 * Returns server runtime name ([type]-[version])
	 * @return server runtime name
	 */
	public String getName() {
		return type + "-" + version; 
	}
	
}
