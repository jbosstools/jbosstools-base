package org.jboss.tools.ui.bot.ext.config;

public class JavaBean extends RuntimeBean {
	
	public JavaBean() {
		this.key = TestConfigurator.Keys.JAVA;
	}
	public static JavaBean fromString(String propValue) throws Exception {
		return (JavaBean)fromString(propValue, new JavaBean());
	}
}
