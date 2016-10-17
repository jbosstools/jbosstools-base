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

public class UnresolvedClass {
	private String name;
	private String javaVersion;
	
	
	public UnresolvedClass(String name, String javaVersion){
		this.name = name;
		this.javaVersion = javaVersion;
	}
	
	
	public String toString(){
		return NLS.bind(JVMProblemDetectorMessages.UNRESOLVED_CLASS_LABEL, new Object[]{name, javaVersion});
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