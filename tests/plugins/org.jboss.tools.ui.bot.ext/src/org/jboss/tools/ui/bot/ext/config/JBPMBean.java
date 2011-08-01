package org.jboss.tools.ui.bot.ext.config;

public class JBPMBean extends RuntimeBean {

	public JBPMBean() {
		this.key = TestConfigurator.Keys.JBPM;
	}
	public static JBPMBean fromString(String propValue) throws Exception {
		return (JBPMBean)fromString(propValue, new JBPMBean());
	}
}
