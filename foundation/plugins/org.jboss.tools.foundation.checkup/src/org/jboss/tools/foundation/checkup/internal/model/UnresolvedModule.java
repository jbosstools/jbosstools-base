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

import org.eclipse.osgi.util.NLS;
import org.jboss.tools.foundation.checkup.JVMProblemDetectorMessages;

public class UnresolvedModule{
	private String name;
	private String javaName;
	private String javaVersion;
	
	private DependantList list;
	
	public UnresolvedModule(String name, String javaName, String javaVersion){
		this.name = name;
		this.javaName = javaName;
		this.javaVersion = javaVersion;
		list = new DependantList(this);
	}
	
	public DependantList getDependantList(){
		return list;
	}
	
	public String toString(){
		return NLS.bind(JVMProblemDetectorMessages.UNRESOLVED_MODULE_LABEL, new Object[]{name, javaName, javaVersion});
	}

	public boolean equals(Object o) {
		if(o instanceof UnresolvedModule){
			return o.toString().equals(toString());
		}
		return super.equals(o);
	}

	public int hashCode() {
		return toString().hashCode();
	}
}