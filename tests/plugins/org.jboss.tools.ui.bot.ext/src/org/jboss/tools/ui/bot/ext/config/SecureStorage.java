package org.jboss.tools.ui.bot.ext.config;

public class SecureStorage {

	public String password;
	public static SecureStorage fromString(String key, String propValue) throws Exception {
		try {
			if (propValue==null) {
				return null;
			}
			SecureStorage bean = new SecureStorage();
			bean.password = propValue;
			return bean;
			}
			catch (Exception ex) {
				throw new Exception(String.format("Cannot parse %s property line",key),ex);
			}
	}
	@Override
	public String toString() {
		return String.format("Secure Storage is configured");
	}
}
