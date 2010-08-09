package org.jboss.tools.ui.bot.ext.config;

public class ESBBean {
	public String version;
	public String esbHome;
	
	public static ESBBean fromString(String propValue) throws Exception{
		try {
			if (propValue==null) {
				return null;
			}
			String[] esbParams = propValue.split(",");
			ESBBean bean = new ESBBean();
			bean.esbHome=esbParams[1];
			bean.version=esbParams[0];
			return bean;
			}
			catch (Exception ex) {
				throw new Exception("Cannot parse ESB property line",ex);
			}
	}
	@Override
	public String toString() {
		return String.format("ESB runtime version=%s, home=%s",
				this.version, this.esbHome);
	}
}
