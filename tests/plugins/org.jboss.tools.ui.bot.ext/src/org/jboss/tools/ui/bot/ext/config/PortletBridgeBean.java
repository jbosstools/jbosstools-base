package org.jboss.tools.ui.bot.ext.config;


/**
 * Configuration of portlet bridge. 
 * 
 * @author Lucia Jelinkova
 *
 */
public class PortletBridgeBean {

	private String location;

	public static PortletBridgeBean fromString(String propValue) throws Exception {
		if (propValue==null) {
			return null;
		}
		
		PortletBridgeBean bean = new PortletBridgeBean();
		bean.location = propValue;
		return bean;
	}
	
	public String getLocation() {
		return location;
	}
	
	@Override
	public String toString() {
		return "Portlet Bridge runtime: " + location;
	}
}
