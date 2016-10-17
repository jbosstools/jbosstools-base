/******************************************************************************* 
 * Copyright (c) 2016 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.foundation.checkup.internal.log;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.eclipse.osgi.internal.messages.Msg;
import org.eclipse.ui.internal.views.log.LogSession;
import org.jboss.tools.foundation.checkup.internal.model.JVMProblemModel;
import org.jboss.tools.foundation.checkup.internal.model.UnresolvedStructure;
import org.osgi.framework.BundleException;
import org.osgi.framework.Constants;

public class LineScanner {
	// org.osgi.framework.BundleException: Could not resolve module: 
	private static final String BUNDLE_EXCEPTION_STRING = BundleException.class.getCanonicalName()+": "+Msg.Module_ResolveError; //$NON-NLS-1$
	
	// -> Bundle-SymbolicName:
	private static final String BUNDLE_SYMBOLICNAME_STRING = "-> "+Constants.BUNDLE_SYMBOLICNAME+":"; //$NON-NLS-1$
	
	// Unresolved requirement: Require-Capability: : osgi.ee; filter:=\"(&(osgi.ee=
	private static final String UNRESOLVED_REQUIREMENT_STRING = Msg.ModuleResolutionReport_UnresolvedReq+Constants.REQUIRE_CAPABILITY + ": osgi.ee; filter:=\"(&(osgi.ee="; //$NON-NLS-1$
	private static final String VERSION_STRING = ")(version="; //$NON-NLS-1$
	
	// Caused by: java.lang.UnsupportedClassVersionError: org/jboss/tools/usage/event/UsageEventType : Unsupported major.minor version 52.0
	private static final String UNSUPPORTED_CLASS_VERSION_STRING = UnsupportedClassVersionError.class.getCanonicalName()+": ";//$NON-NLS-1$
	
	private static final String UNSUPPORTED_MAJOR_MINOR_VERSION_STRING = ": Unsupported major.minor version";//$NON-NLS-1$
	
	private static HashMap<String, String> majorMinorVersions = new HashMap<String, String>();
	
	static{
		majorMinorVersions.put("45.3", "1.1");
		majorMinorVersions.put("46.0", "1.2");
		majorMinorVersions.put("47.0", "1.3");
		majorMinorVersions.put("48.0", "1.4");
		majorMinorVersions.put("49.0", "1.5");
		majorMinorVersions.put("50.0", "1.6");
		majorMinorVersions.put("51.0", "1.7");
		majorMinorVersions.put("52.0", "1.8");
	}
	
	// temporary storage
	List<String> moduleNameList = new ArrayList<String>();
	String currentModuleName = null;
	UnresolvedStructure structure;
	JVMProblemModel model;
	public LineScanner(JVMProblemModel model) {
		this.model = model;
		this.structure = model.getStructure();
	}
	public void scanLine(String line){
		if (line.startsWith(LogSession.SESSION)) {
			Date date = JVMProblemLogUtil.getSessionDate(line);
			if(date != null){
				model.setCurrentDate(date);
			}
			// clear previous data
			// call from NOT UI Thread
			structure.clear();
			moduleNameList.clear();
			currentModuleName = null;
		} else if (line.startsWith("!ENTRY")) {
			Date date = JVMProblemLogUtil.getEntryDate(line);
			model.setCurrentDate(date);
		} else if(line.startsWith(BUNDLE_EXCEPTION_STRING)){
			if(isInCurrentSession()){
				// parse unresolved module
				int position = line.indexOf("[");
				String unresolvedModule;
				if(position > 0){
					unresolvedModule = line.substring(BUNDLE_EXCEPTION_STRING.length(), position).trim();
				}else{
					unresolvedModule = line.substring(BUNDLE_EXCEPTION_STRING.length()).trim();
				}
				
				moduleNameList.clear();
				currentModuleName = unresolvedModule;
			}
		} else if(line.startsWith(BUNDLE_SYMBOLICNAME_STRING)){
			if(isInCurrentSession()){
				// parse unresolved module
				int position = line.indexOf(";");
				String unresolvedModule;
				if(position > 0){
					unresolvedModule = line.substring(BUNDLE_SYMBOLICNAME_STRING.length(), position).trim();
				}else{
					unresolvedModule = line.substring(BUNDLE_SYMBOLICNAME_STRING.length()).trim();
				}
				
				if(currentModuleName != null && !moduleNameList.contains(currentModuleName)){
					moduleNameList.add(currentModuleName);
				}
				currentModuleName = unresolvedModule;
			}
		} else if(line.startsWith(UNRESOLVED_REQUIREMENT_STRING)){
			if(isInCurrentSession()){
				// parse Java name and version
				int position = line.indexOf(VERSION_STRING);
				if(position > 0){
					int endPosition = line.indexOf(")", position+VERSION_STRING.length());
					
					String javaName = line.substring(UNRESOLVED_REQUIREMENT_STRING.length(), position).trim();
					String javaVersion;
					if(endPosition > 0){
						javaVersion = line.substring(position+VERSION_STRING.length(), endPosition).trim();
					}else{
						javaVersion = line.substring(position+VERSION_STRING.length()).trim();
					}
					// call from NOT UI Thread
					// store unresolved module
					structure.addRequieredJava(currentModuleName, moduleNameList, javaName, javaVersion);
				}
			}
		}else if(line.indexOf(UNSUPPORTED_CLASS_VERSION_STRING) >= 0){
			if(isInCurrentSession()){
				int position = line.indexOf(UNSUPPORTED_CLASS_VERSION_STRING);
				int versionPosition = line.indexOf(UNSUPPORTED_MAJOR_MINOR_VERSION_STRING);
				if(versionPosition >= 0){
					String className = line.substring(position+UNSUPPORTED_CLASS_VERSION_STRING.length(), versionPosition).trim();
					String majorMinorVersion = line.substring(versionPosition+UNSUPPORTED_MAJOR_MINOR_VERSION_STRING.length()).trim();
					String javaVersion = majorMinorVersions.get(majorMinorVersion);
					if(javaVersion != null){
						structure.addUnresolvedClass(className, javaVersion);
					}
				}
			}
		}
	}

	private boolean isInCurrentSession(){
		return 	model.getCurrentDate() != null &&
				model.getCurrentDate().getTime() >= 
					model.getEclipseStartTime();
	}
	
}
