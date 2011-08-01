package org.jboss.tools.ui.bot.ext.config;

public class SeamBean extends RuntimeBean{
	
	public SeamBean() {
		this.key = TestConfigurator.Keys.SEAM;
	}
	public static SeamBean fromString(String propValue) throws Exception {
		return (SeamBean)fromString(propValue, new SeamBean());
	}
}
