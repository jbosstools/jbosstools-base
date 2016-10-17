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

import org.jboss.tools.foundation.checkup.JVMProblemDetectorMessages;

public class DependantList {
	private UnresolvedModule module;
	// list of Dependant
	private List<Dependant> dependants = new ArrayList<Dependant>();
	
	public DependantList(UnresolvedModule module){
		this.module = module;
	}
	
	public UnresolvedModule getUnresolvedModule(){
		return module;
	}
	
	public String toString(){
		return module.toString()+" "+JVMProblemDetectorMessages.DEPENDANT_MODULES;
	}
	
	public List<Dependant> getDependants(){
		return dependants;
	}
	
	
	public void add(Dependant dependant){
		synchronized(dependants){
			if(!dependants.contains(dependant)){
				dependants.add(dependant);
			}
		}
	}
	
	public boolean equals(Object o) {
		if(o instanceof Dependant){
			return o.toString().equals(toString());
		}
		return super.equals(o);
	}
	
	public int hashCode() {
		return toString().hashCode();
	}
}