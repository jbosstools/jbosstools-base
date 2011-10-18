package org.jboss.tools.common.model.filesystems.impl;

import java.util.HashMap;
import java.util.Map;

public class JarAccessFactory {
	
	private static Map<String, JarAccess> jars = new HashMap<String, JarAccess>();

	public synchronized static JarAccess getJarAccess(String location, JarSystemImpl context) {
		JarAccess jar = jars.get(location);
		if(jar == null) {
			jar = new JarAccess();
			jar.setMain(context);
			jar.setLocation(location);
			jars.put(location, jar);
		}
		if(context != jar.getMain()) {
			jar.addSlave(context);
		}
		return jar;
	}

}
