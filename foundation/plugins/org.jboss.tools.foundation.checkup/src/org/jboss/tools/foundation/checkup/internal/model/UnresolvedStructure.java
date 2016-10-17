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
package org.jboss.tools.foundation.checkup.internal.model;

import java.util.ArrayList;
import java.util.List;

public class UnresolvedStructure{
	private List<UnresolvedModule> unresolvedModuleList = new ArrayList<UnresolvedModule>();
	
	private List<UnresolvedClass> unresolvedClassList = new ArrayList<UnresolvedClass>();
	
	private JVMProblemModel model;
	public UnresolvedStructure(JVMProblemModel model) {
		this.model = model;
	}
	
	/**
	 * returns copy of list of unresolved module
	 * supposed to be called from UI Thread
	 * @return
	 */
	public List<UnresolvedModule> getUnresolvedModules(){
		synchronized(unresolvedModuleList){
			List<UnresolvedModule> list = new ArrayList<UnresolvedModule>(unresolvedModuleList);
			if(!model.isTestEnvironment()){
				unresolvedModuleList.clear();
			}
			return list;
		}
	}

	/**
	 * returns copy of list of unresolved classes
	 * supposed to be called from UI Thread
	 * @return
	 */
	public List<UnresolvedClass> getUnresolvedClasses(){
		synchronized(unresolvedClassList){
			List<UnresolvedClass> list = new ArrayList<UnresolvedClass>(unresolvedClassList);
			if(!model.isTestEnvironment()){
				unresolvedClassList.clear();
			}
			return list;
		}
	}
	

	public boolean isNeedReport(){
		if(model.isTestEnvironment()){
			return false;
		}
		synchronized(unresolvedModuleList){
			if( unresolvedModuleList.size() > 0)
				return true;
		}
		synchronized(unresolvedClassList){
			return unresolvedClassList.size() > 0;
		}
	}
	
	public void clear(){
		synchronized(unresolvedModuleList){
			unresolvedModuleList.clear();
		}
		synchronized(unresolvedClassList){
			unresolvedClassList.clear();
		}
	}
	
	public void addUnresolvedClass(String className, String javaVersion){
		synchronized(unresolvedClassList){
			UnresolvedClass unresolvedClass = new UnresolvedClass(className, javaVersion);
			
			if(!unresolvedClassList.contains(unresolvedClass)){
				unresolvedClassList.add(unresolvedClass);
			}
		}

	}
	
	
	public void addRequieredJava(String moduleName, List<String> moduleNameList, String javaName, String javaVersion){
		if(moduleName == null){
			return;
		}
		
		synchronized(unresolvedModuleList){
			UnresolvedModule unresolvedModule = new UnresolvedModule(moduleName, javaName, javaVersion);
			
			if(unresolvedModuleList.contains(unresolvedModule)){
				for(UnresolvedModule module : unresolvedModuleList){
					if(module.equals(unresolvedModule)){
						unresolvedModule = module;
						break;
					}
				}
			}else{
				unresolvedModuleList.add(unresolvedModule);
			}
			
			DependantList dependantList = unresolvedModule.getDependantList();
			for(String name : moduleNameList){
				Dependant dep = new Dependant(unresolvedModule, name);
				dependantList.add(dep);
			}
			
			moduleNameList.clear();
		}
	}
	
}