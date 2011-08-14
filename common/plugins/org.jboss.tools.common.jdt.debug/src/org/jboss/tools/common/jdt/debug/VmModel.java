/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author snjeza
 *
 */
public class VmModel {
	private String pid;
	private String port;
	private String transport;
	private String jvmArgs;
	private String mainClass;
	private String mainArgs;
	private Map<String, String> args;
	private String jdwpArgs;
	private String displayName;

	public String getJvmArgs() {
		return jvmArgs;
	}

	public void setJvmArgs(String jvmArgs) {
		this.jvmArgs = jvmArgs;
		args = getArguments(jvmArgs);
		jdwpArgs = getJdwpArg(args);
	}

	public String getMainClass() {
		return mainClass;
	}

	public void setMainClass(String mainClass) {
		this.mainClass = mainClass;
	}

	public String getMainArgs() {
		return mainArgs;
	}

	public void setMainArgs(String mainArgs) {
		this.mainArgs = mainArgs;
	}

	private Map<String, String> getArguments(String args) {
		Map<String, String> arguments = new HashMap<String, String>();

		for (String argument : args.split(" ")) { //$NON-NLS-1$
			if (argument.length() == 0) {
				continue;
			}
			String[] nameValue = argument.split("=", 2); //$NON-NLS-1$
			if (nameValue.length == 1) {
				arguments.put(nameValue[0], nameValue[0]);
			} else {
				arguments.put(nameValue[0], nameValue[1]);
			}

		}
		return arguments;
	}

	private String getJdwpArg(Map<String, String> arguments) {
		String jdwpArg = arguments.get("-agentlib:jdwp"); //$NON-NLS-1$
		if (jdwpArg == null) {
			jdwpArg = arguments.get("-Xrunjdwp:transport"); //$NON-NLS-1$
		}
		return jdwpArg;
	}
	
	public String getPort() {
		if (this.port != null) {
			return this.port;
		}
		if (jdwpArgs == null) {
			return null;
		}
		for (String argument : jdwpArgs.split(",")) { //$NON-NLS-1$
			String[] nameValue = argument.split("=", 2); //$NON-NLS-1$
			if (nameValue.length == 2) {
				if ("address".equals(nameValue[0])) { //$NON-NLS-1$
					String address = nameValue[1];
					if (address == null) {
						continue;
					}
					String[] addresses = address.split(":", 2); //$NON-NLS-1$
					if (addresses.length == 1) {
						port = addresses[0];
						return port;
					} else {
						port = addresses[1];
						return port;
					}
				}
			}
		}
		return null;
	}

	public String getTransport() {
		if (this.transport != null) {
			return this.transport;
		}
		if (jdwpArgs == null) {
			return null;
		}
		for (String argument : jdwpArgs.split(",")) { //$NON-NLS-1$
			String[] nameValue = argument.split("=", 2); //$NON-NLS-1$
			if (nameValue.length == 2) {
				if ("transport".equals(nameValue[0])) { //$NON-NLS-1$
					this.transport = nameValue[1];
					return transport;
				}
			}
			if (nameValue.length == 1) {
				if (RemoteDebugActivator.DT_SOCKET.equals(nameValue[0])) { 
					this.transport = nameValue[0];
					return transport;
				}
			}
		}
		return null;
	}
	
	public String getDisplayName() {
		if (displayName == null) {
			StringBuffer buffer = new StringBuffer();
			if (getMainClass() != null) {
				buffer.append(mainClass);
//				if (mainArgs != null) {
//					if (buffer.length() > 0) {
//						buffer.append(" ");
//					}
//					buffer.append(mainArgs);
//				}
				if (pid != null) {
					if (buffer.length() > 0) {
						buffer.append(",pid="); //$NON-NLS-1$
					}
					buffer.append(pid);
				}
				String port = getPort();
				if (port != null) {
					if (buffer.length() > 0) {
						buffer.append(",port="); //$NON-NLS-1$
					}
					buffer.append(port);
				}
			}
			if (buffer.length() > 0) {
				displayName = buffer.toString();
			}
		}
		return displayName;
	}

	public String getPid() {
		return pid;
	}

	public void setPid(String pid) {
		this.pid = pid;
	}

	public void setPort(String port) {
		this.port = port;
	}

	public void setTransport(String transport) {
		this.transport = transport;
	}
	
	@Override
	public String toString() {
		return "VmModel [pid=" + pid + ", port=" + port + ", transport="
				+ transport + ", jvmArgs=" + jvmArgs + ", mainClass="
				+ mainClass + ", mainArgs=" + mainArgs + ", args=" + args
				+ ", jdwpArgs=" + jdwpArgs + ", displayName=" + displayName
				+ "]";
	}

}
