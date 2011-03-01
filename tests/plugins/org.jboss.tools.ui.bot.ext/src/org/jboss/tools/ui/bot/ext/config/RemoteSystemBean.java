package org.jboss.tools.ui.bot.ext.config;

public class RemoteSystemBean {
	public String user;
	public String host;
	public String key;
	public static RemoteSystemBean fromString(String propValue) throws Exception {
		try {
			if (propValue == null) {
				return null;
			}
			String[] rsParams = propValue.split(",");
			RemoteSystemBean bean = new RemoteSystemBean();
			bean.user = rsParams[0];
			bean.host = rsParams[1];
			bean.key = rsParams[2];
			return bean;
		} catch (Exception ex) {
			throw new Exception("Cannot parse RS property line="+propValue, ex);
		}
	}

	@Override
	public String toString() {
		return String.format("Remote System user=%s, host=%s, key=%s",
				this.user, this.host, this.key);
	}
}
