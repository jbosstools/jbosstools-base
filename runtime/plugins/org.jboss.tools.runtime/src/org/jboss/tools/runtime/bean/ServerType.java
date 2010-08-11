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

public class ServerType {
	
	private static final String JBOSS_AS_PATH = "jboss-as";
	private static final String BIN_PATH = "bin";
	private static final String TWIDDLE_JAR_NAME = "twiddle.jar";
	private static final String RUN_JAR_NAME = "run.jar";
	private static final String JBOSS_ESB_PATH = "jboss-esb";
	private static final String SOAP_JBPM_JPDL_PATH = "jbpm-jpdl";
	private static final String JBOSS_AS_WEB_PATH = "jboss-as-web";
	private static final String JBOSS_PORTLETBRIDGE_PATH = "portletbridge";
	
	private static String UNKNOWN_STR = "UNKNOWN";
	
	private String name;
	private String jbossSystemJarPath;
	private String[] versions = new String[0];
	
	private ServerType.Condition condition = null;
	private String id=UNKNOWN_STR;
	
	protected ServerType(String id, String name, String jbossSystemJarPath, String[] versions, Condition condition) {
		this.id = id;
		this.name = name;
		this.jbossSystemJarPath = jbossSystemJarPath;
		this.versions = versions;
		this.condition = condition;
	}

	public static final ServerType AS = new ServerType(
			"AS",
			"Application Server",
			BIN_PATH+File.separatorChar + TWIDDLE_JAR_NAME,
			new String[]{"6.0","5.1", "5.0", "4.2", "4.0", "3.2"}, new ASServerTypeCondition());
	
	public static final ServerType EAP = new ServerType(
			"EAP",
			"Enterprise Application Platform",
			JBOSS_AS_PATH + File.separatorChar + BIN_PATH+ File.separatorChar + TWIDDLE_JAR_NAME, 
			new String[]{"4.2","4.3","5.0"}, new EAPServerTypeCondition());
	
	public static final ServerType SOAP = new ServerType(
			"SOA-P",
			"SOA Platform",
			JBOSS_AS_PATH + File.separatorChar + BIN_PATH+ File.separatorChar + TWIDDLE_JAR_NAME,
			new String[]{"4.3","5.0"}, new SOAPServerTypeCondition());

	public static final ServerType SOAP_STD = new ServerType(
			"SOA-P-STD",
			"SOA Platform Standalone",
			JBOSS_ESB_PATH + File.separatorChar + BIN_PATH+ File.separatorChar + RUN_JAR_NAME,
			new String[]{"4.3","5.0"}, new SOAPStandaloneServerTypeCondition());

	public static final ServerType EWP = new ServerType( 
			"EWP",
			"Enterprise Web Platform",
			JBOSS_AS_WEB_PATH + File.separatorChar + BIN_PATH + File.separatorChar + RUN_JAR_NAME,
			new String[]{"5.0"}, new EWPTypeCondition());
	
	public static final ServerType EPP = new ServerType( 
			"EPP",
			"Enterprise Portal Platform",
			JBOSS_AS_PATH + File.separatorChar + BIN_PATH + File.separatorChar + RUN_JAR_NAME,
			new String[]{"4.3","5.0"}, new EPPTypeCondition());
	
	public static final ServerType UNKNOWN = new ServerType(
			UNKNOWN_STR,
			UNKNOWN_STR,
			"",
			new String[]{"6.0", "5.1", "5.0", "4.3", "4.2", "4.0", "3.2"}, null);

	public String toString() {
		return id;
	}
	
	public static ServerType getType(String name) {
		if(AS.name.equals(name)) {
			return AS;
		} else if(EAP.name.equals(name)) {
			return EAP;
		} else if(SOAP.name.equals(name)) {
			return SOAP;
		} else if(SOAP_STD.name.equals(name)) {
			return SOAP_STD;
		} else if(EWP.name.equals(name)) {
			return EWP;
		} else if(EPP.name.equals(name)) {
			return EPP;
		}
		
		throw new IllegalArgumentException("Name '" + name + "' cannot be converted to ServerType");
	}

	public String[] getVersions() {
		return versions;
	}
	
	public String getName() {
		return name;
	}
	
	public String getSystemJarPath() {
		return jbossSystemJarPath;
	}
	
	public boolean isServerRoot(File location) {
		return this.condition.isServerRoot(location);
	}
	
	public static final ServerType[] KNOWN_TYPES = {AS, EAP, SOAP, SOAP_STD, EWP, EPP};

	static interface Condition {
		public boolean isServerRoot(File location);
	}
	
	
	public static class EAPServerTypeCondition implements Condition {
		public boolean isServerRoot(File location) {
			File asSystemJar = new File(location, ServerType.EAP.getSystemJarPath());
			return asSystemJar.exists() && asSystemJar.isFile();
		}
	}
	
	public static class ASServerTypeCondition implements Condition {
		
		public boolean isServerRoot(File location) {
			File asSystemJar = new File(location, ServerType.AS.getSystemJarPath());
			return asSystemJar.exists() && asSystemJar.isFile();
		}
	}
	
	public static class SOAPServerTypeCondition extends EAPServerTypeCondition{
		
		public boolean isServerRoot(File location) {
			File jbpmFolder = new File(location, SOAP_JBPM_JPDL_PATH);
			return super.isServerRoot(location) && jbpmFolder.exists() && jbpmFolder.isDirectory();
		}
	}

	public static class SOAPStandaloneServerTypeCondition implements Condition {
		
		public boolean isServerRoot(File location) {
			File jbpmFolder = new File(location, SOAP_JBPM_JPDL_PATH);
			File soaStdSystemJar = new File(location,JBOSS_ESB_PATH + File.separatorChar + BIN_PATH + File.separatorChar + RUN_JAR_NAME);			
			return 
				jbpmFolder.exists() && jbpmFolder.isDirectory() 
					&& 
				soaStdSystemJar.exists() && soaStdSystemJar.isFile();
		}
	}
	
	public static class EWPTypeCondition implements Condition {
		public boolean isServerRoot(File location) {
			File ewpSystemJar = new File(location,ServerType.EWP.getSystemJarPath());
			return ewpSystemJar.exists() && ewpSystemJar.isFile();
		}
	}
	
	public static class EPPTypeCondition implements Condition {
		public boolean isServerRoot(File location) {
			File portletBridgeFolder = new File(location, JBOSS_PORTLETBRIDGE_PATH);
			File portlalSarFolder = new File(location, JBOSS_AS_PATH + File.separatorChar + "server" + File.separatorChar + "default" + File.separatorChar + "deploy" + File.separatorChar + "jboss-portal.sar" );			
			File asStdSystemJar = new File(location,JBOSS_AS_PATH + File.separatorChar + BIN_PATH + File.separatorChar + RUN_JAR_NAME);			
			return 
				(portletBridgeFolder.exists() && portletBridgeFolder.isDirectory() 
					||
					portlalSarFolder.exists() && portlalSarFolder.isDirectory())
					&& 
				asStdSystemJar.exists() && asStdSystemJar.isFile();
		}
	}

	public String getId() {
		return id;
	}
}