package org.jboss.tools.ui.bot.ext.config;

public class JBPMBean {
	public String version;
	public String jbpmHome;
	
	public static JBPMBean fromString(String propValue) throws Exception{
		try {
			if (propValue==null) {
				return null;
			}
			String[] jbpmParams = propValue.split(",");
			JBPMBean bean = new JBPMBean();
			bean.jbpmHome=jbpmParams[1];
			bean.version=jbpmParams[0];
			return bean;
			}
			catch (Exception ex) {
				throw new Exception("Cannot parse jBPM property line",ex);
			}
	}
	@Override
	public String toString() {
		return String.format("jBPM runtime version=%s, home=%s",
				this.version, this.jbpmHome);
	}
}
