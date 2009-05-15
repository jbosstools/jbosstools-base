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
package org.jboss.tools.common.model.project.ext.event;

/**
 * This interface allows to process SeamProjectChangeEvent, or specific Change by
 * invoking method visit(IChangeVisitor) declared on them.
 * 
 * @author Viacheslav Kabanovich
 */
public interface IChangeVisitor {
	
	/**
	 * Processes a single change. If this method returns true,
	 * it shall be invoked with child changes
	 * @param change
	 * @return
	 */
	public boolean visit(Change change);
}
