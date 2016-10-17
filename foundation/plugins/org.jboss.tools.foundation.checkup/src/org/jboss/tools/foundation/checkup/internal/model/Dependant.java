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

public class Dependant {
	private UnresolvedModule module;
	private String name;
	
	public Dependant(UnresolvedModule module, String name){
		this.name = name;
		this.module = module;
	}
	
	public UnresolvedModule getParent(){
		return module;
	}
	
	public String toString(){
		return name;
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