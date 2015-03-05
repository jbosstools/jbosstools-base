/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.internal;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.VmModel;

public class VmModelCommandUtility {
	private static boolean loggedWarning = false;
	private static String[] WIN_CMD_ARRAY = { "netstat", "-ona", "-p", "tcp" };
	private static String[] MAC_CMD_ARRAY = { "lsof", "-i", "-P" };
	private static String[] LINUX_CMD_ARRAY = { "netstat", "-nlt", "-p", "" };
	
	public static VmModel getVmModelUsingOsCommand(Integer vmPid, IProgressMonitor monitor) {
		if (vmPid == null) {
			return null;
		}
		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}
		if (monitor.isCanceled()) {
			return null;
		}
		String vmPidStr = String.valueOf(vmPid);
		String[] cmdarray = getOsCommand(vmPidStr);
		if (cmdarray == null) {
			return null;
		}
		InputStream is = null;
		BufferedReader reader = null;
		try {
			Process process = Runtime.getRuntime().exec(cmdarray);
			if (monitor.isCanceled()) {
				return null;
			}
			is = process.getInputStream();
			reader = new BufferedReader(new InputStreamReader(is));
			String line = reader.readLine();
			
			while (line != null) {
				if (line.contains("LISTEN")) {
					line = clearWhiteSpace(line);
					VmModel model = processLine(line, vmPidStr);
					if (model != null) {
						return model;
					}
				}
				if (monitor.isCanceled()) {
					return null;
				}
				line = reader.readLine();
			}
		} catch (IOException e) {
			if( !loggedWarning ) {
				RemoteDebugActivator.pluginLog().logWarning(e);
				loggedWarning = true;
			}
		} finally {
			if (reader != null) {
				try {
					reader.close();
				} catch (IOException e1) {
					// ignore
				}
			}
			if (is != null) {
				try {
					is.close();
				} catch (IOException e1) {
					// ignore
				}
			}
		}

		return null;
	}


	private static String[] getOsCommand(String pid) {
		if (Platform.OS_WIN32.equals(Platform.getOS())) {
			return WIN_CMD_ARRAY;
		} else if (Platform.OS_MACOSX.equals(Platform.getOS())) {
			return MAC_CMD_ARRAY;
		} else if (Platform.OS_LINUX.equals(Platform.getOS())) {
			String[] cmdarray = LINUX_CMD_ARRAY;
			cmdarray[cmdarray.length-1] = pid;
			return cmdarray;
		}
		return null;
	}

	
	private static VmModel processLine(String line, String vmPidStr) {
		if (Platform.OS_WIN32.equals(Platform.getOS())) {
			return processWinLine(line, vmPidStr);
		} else if (Platform.OS_MACOSX.equals(Platform.getOS())) {
			return processMacLine(line, vmPidStr);
		} else if (Platform.OS_LINUX.equals(Platform.getOS())) {
			return processLinuxLine(line, vmPidStr);
		}
		return null;
	}

	private static VmModel processLinuxLine(String line, String vmPidStr) {
		String[] elements = line.split(" ", 7);
		if (elements.length >= 7 &&
				elements[0] != null && elements[0].equals("tcp")) {
			if (elements[6] != null) {
				String[] pids = elements[6].split("/");
				String pid;
				if (pids.length == 2) {
					pid = pids[0];
					if (pid != null && pid.equals(vmPidStr)) {
						if (elements[3] != null) {
							String port = getPort(elements[3]);
							if (port != null) {
								VmModel model = new VmModel();
								model.setPid(vmPidStr);
								model.setMainClass(RemoteDebugActivator.UNKNOWN);
								model.setPort(port);
								model.setTransport(RemoteDebugActivator.DT_SOCKET);
								return model;
							}
						}
					}
				}
			}
		}
		return null;
	}


	private static VmModel processWinLine(String line, String vmPidStr) {
		String[] elements = line.split(" ", 5);
		if (elements.length == 5 && elements[4] != null && elements[4].equals(vmPidStr)) {
			if (elements[1] != null) {
				String port = getPort(elements[1]);
				if (port != null && !port.isEmpty()) {
					VmModel model = new VmModel();
					model.setPid(vmPidStr);
					model.setMainClass(RemoteDebugActivator.UNKNOWN);
					model.setPort(port);
					model.setTransport(RemoteDebugActivator.DT_SOCKET);
					return model;
				}
			}
		}
		return null;
	}
	
	private static VmModel processMacLine(String line, String vmPidStr) {
		String[] elements = line.split(" ", 10);
		if (elements.length >= 10 &&
				elements[1] != null && elements[1].equals(vmPidStr) && 
				elements[7] != null && elements[7].equals("TCP")) {
			if (elements[8] != null) {
				String port = getPort(elements[8]);
				if (port != null && !port.isEmpty()) {
					VmModel model = new VmModel();
					model.setPid(vmPidStr);
					model.setMainClass(RemoteDebugActivator.UNKNOWN);
					model.setPort(port);
					model.setTransport(RemoteDebugActivator.DT_SOCKET);
					return model;
				}
			}
		}
		return null;
	}


	private static String getPort(String element) {
		String[] ports = element.split(":");
		String port;
		if (ports.length == 2) {
			port = ports[1];
		} else {
			port = ports[0];
		}
		return port;
	}


	private static String clearWhiteSpace(String line) {
		line = line.trim();
		while (line.contains("\t")) {
			line = line.replace(" ", "\t");
		}
		while (line.contains("  ")) {
			line = line.replace("  ", " ");
		}
		return line;
	}

}
