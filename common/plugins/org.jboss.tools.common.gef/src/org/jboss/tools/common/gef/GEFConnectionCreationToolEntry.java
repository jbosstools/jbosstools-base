/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.gef;

import org.eclipse.gef.Tool;
import org.eclipse.gef.palette.ConnectionCreationToolEntry;
import org.eclipse.gef.requests.CreationFactory;
import org.eclipse.gef.tools.ConnectionCreationTool;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.dnd.DragSourceEvent;

/**
 * 
 * @author eskimo(dgolovin@exadel.com)
 *
 */
public class GEFConnectionCreationToolEntry extends ConnectionCreationToolEntry {
	
	/*
	 * 
	 */
	private ConnectionCreationTool tool=null;
	
	/*
	 * 
	 */
	private boolean flag=false;

	/**
	 * 
	 * @param label
	 * @param shortDesc
	 * @param factory
	 * @param iconSmall
	 * @param iconLarge
	 */
	public GEFConnectionCreationToolEntry(
		String label,
		String shortDesc,
		CreationFactory factory,
		ImageDescriptor iconSmall,
		ImageDescriptor iconLarge) {
		super(label, shortDesc, factory, iconSmall, iconLarge);
	}
	
	/**
	 * Create new Tool
	 */
	public Tool createTool() {
		tool = new CustomConnectionCreationTool(factory);
		tool.setUnloadWhenFinished(flag);
		return tool;
	}

	/**
	 * 
	 * @param flag
	 */
	public void setUnloadWhenFinished(boolean flag){
		this.flag = flag;
		
	}
	
	/**
	 * 
	 * @author eskimo(dgolovin@exadel.com)
	 *
	 */
	class CustomConnectionCreationTool extends ConnectionCreationTool{
		public CustomConnectionCreationTool(CreationFactory factory){
			super(factory);
		}
		 
		/**
		 * 	
		 */ 
		protected void handleFinished(){
			dragFinished();
			if (flag)
				getDomain().loadDefaultTool();
			else
				reactivate();
		}
		
	}
	
	protected void dragFinished() { // TODO-3.3: change?
	}
}
