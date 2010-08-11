/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 

package org.jboss.tools.runtime.bean;

import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * @author eskimo
 *
 */
public class ServerBeanLoader {
	
	public static final String SOAP_JBPM_JPDL_PATH = "jbpm-jpdl";

	public ServerBean loadFromLocation(File location) {
		ServerType type = getServerType(location);
		String version = getServerVersion(getFullServerVersion(new File(location,type.getSystemJarPath())));
		ServerBean server = new ServerBean(location.getPath(),getName(location),type,version);
		return server;
	}
	
	public ServerType getServerType(File location) {
		if(ServerType.AS.isServerRoot(location)) {
			return ServerType.AS;
		} else if(ServerType.EAP.isServerRoot(location) && ServerType.SOAP.isServerRoot(location)) {
			return ServerType.SOAP;
		} else if(ServerType.SOAP_STD.isServerRoot(location)) {
			return ServerType.SOAP_STD;
		} else if(ServerType.EAP.isServerRoot(location) && ServerType.EPP.isServerRoot(location)) {
			return ServerType.EPP;
		} else if(ServerType.EAP.isServerRoot(location)) {
			return ServerType.EAP;
		} else if(ServerType.EWP.isServerRoot(location)) {
			return ServerType.EWP;
		}
		return ServerType.UNKNOWN;
	}
	
	public String getName(File location) {
		return location.getName();
	}
	
	public String getFullServerVersion(File systemJarFile) {
		String version = null;
		if(systemJarFile.canRead()) {
			try {
				ZipFile jar = new ZipFile(systemJarFile);
				ZipEntry manifest = jar.getEntry("META-INF/MANIFEST.MF");
				Properties props = new Properties();
				props.load(jar.getInputStream(manifest));
				version = (String)props.get("Specification-Version");
			} catch (IOException e) {
				// version = ""
			}
		}
		return version;
	}
	
	public String getServerVersion(String version) {
		if(version==null) return "";
		String[] versions = ServerType.UNKNOWN.getVersions();
		String adapterVersion = "";
		//  trying to match adapter version by X.X version
		for (String currentVersion : versions) {
			String pattern = currentVersion.replace(".", "\\.") + ".*";
			if(version.matches(pattern)) {
				adapterVersion = currentVersion;
				break;
			}
		}
		
		if("".equals(adapterVersion)) {
			// trying to match by major version
			for (String currentVersion : versions) {
				String pattern = currentVersion.substring(0, 2).replace(".", "\\.") + ".*";
				if(version.matches(pattern)) {
					adapterVersion = currentVersion;
					break;
				}
			}
		}
		return adapterVersion;
	}
	
	public String getAdapterVersion(String version) {
		String[] versions = ServerType.UNKNOWN.getVersions();
		String adapterVersion = "";
		//  trying to match adapter version by X.X version
		for (String currentVersion : versions) {
			String pattern = currentVersion.replace(".", "\\.") + ".*";
			if(version.matches(pattern)) {
				adapterVersion = currentVersion;
				break;
			}
		}
		
		if("".equals(adapterVersion)) {
			// trying to match by major version
			for (String currentVersion : versions) {
				String pattern = currentVersion.substring(0, 2).replace(".", "\\.") + ".*";
				if(version.matches(pattern)) {
					adapterVersion = currentVersion;
					break;
				}
			}
		}
		return adapterVersion;
	}
}
