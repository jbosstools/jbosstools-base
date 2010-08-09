package org.jboss.tools.ui.bot.ext.config;

public class JavaBean {
	public String version;
	public String javaHome;
	
	public static JavaBean fromString(String propValue) throws Exception{
		try {
			if (propValue==null) {
				return null;
			}
			String[] esbParams = propValue.split(",");
			JavaBean bean = new JavaBean();
			bean.javaHome=esbParams[1];
			bean.version=esbParams[0];
			return bean;
			}
			catch (Exception ex) {
				throw new Exception("Cannot parse JAVA property line",ex);
			}
	}
	@Override
	public String toString() {
		return String.format("JAVA runtime version=%s, home=%s",
				this.version, this.javaHome);
	}
}
