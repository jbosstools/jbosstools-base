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
package org.jboss.tools.common.model.ui.swt.util;

import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

public class BorderLayout extends Layout{
	/*public static final int CENTER 	= 0;
	public static final int WEST 	= 1;
	public static final int EAST 	= 2;
	public static final int NORTH	= 3;
	public static final int SOUTH	= 4;*/
	
	public int northHeight = SWT.DEFAULT;
	public int southHeight = SWT.DEFAULT;
	public int westWidth = SWT.DEFAULT;
	public int eastWidth = SWT.DEFAULT;
	
	public Control centerComposite = null;
	public Control northComposite = null;
	public Control southComposite = null;
	public Control westComposite = null;
	public Control eastComposite = null;
	
	public BorderLayout() {}
	
	private void validate() {
		if(northComposite != null && northComposite.isDisposed()) northComposite = null;
		if(westComposite != null && westComposite.isDisposed()) westComposite = null;
		if(southComposite != null && southComposite.isDisposed()) southComposite = null;
		if(eastComposite != null && eastComposite.isDisposed()) eastComposite = null;
		if(centerComposite != null && centerComposite.isDisposed()) centerComposite = null;
	}

	protected Point computeSize(Composite composite, int wHint, int hHint, boolean flushCache){
		validate();
		int width=0, height=0;
		int ewHeight = 0;
		if(northComposite != null){
			if(northHeight == SWT.DEFAULT) {
				height += northComposite.computeSize(wHint, SWT.DEFAULT).y;
			} else {
				height += northHeight;
			}
		}
		if(southComposite != null){
			if(southHeight == SWT.DEFAULT) {
				height += southComposite.computeSize(wHint, SWT.DEFAULT).y;
			} else {
				height += southHeight;
			}
		}
		if(westComposite != null){
			if(westWidth == SWT.DEFAULT) {
				width += westComposite.computeSize(SWT.DEFAULT, hHint).x;
			} else {
				width += westWidth;
			}
			ewHeight = westComposite.computeSize(SWT.DEFAULT, hHint).y;
		}
		if(eastComposite != null){
			if(eastWidth == SWT.DEFAULT) {
				width += eastComposite.computeSize(SWT.DEFAULT, hHint).x;
			} else {
				width += eastWidth;
			}
			int eH = eastComposite.computeSize(SWT.DEFAULT, hHint).y;
			if(eH > ewHeight) ewHeight = eH;
		}
		if(centerComposite != null){
			width += centerComposite.computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
			int ch = centerComposite.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
			if(ewHeight < ch) ewHeight = ch;
		}
		height += ewHeight;
		
		return new Point (width, height);
	}
		
	protected void layout(Composite composite, boolean flushCache) {
		int x = composite.getClientArea().x;
		int y = composite.getClientArea().y;
		int width = composite.getClientArea().width;
		int height = composite.getClientArea().height;
		
		int remWidth = width;
		int remHeight = height;
		int remX = composite.getClientArea().x;
		int remY = composite.getClientArea().y;
		
		
		if(northComposite != null){
			int nH = (northHeight == SWT.DEFAULT) 
			? northComposite.computeSize(width, SWT.DEFAULT).y
				: northHeight;
			northComposite.setBounds(x, y, width, nH);
			remHeight -= nH;
			remY += nH;
		}
		if(southComposite != null){
			int sH = (southHeight == SWT.DEFAULT) 
				? southComposite.computeSize(width, SWT.DEFAULT).y
				: southHeight;
			southComposite.setBounds(x, y + height - sH, width, sH);
			remHeight -= sH;
		}
		if(westComposite != null){
			int wW = (westWidth == SWT.DEFAULT) 
				? westComposite.computeSize(SWT.DEFAULT, height).x
				: westWidth;			
			westComposite.setBounds(x, remY, wW, remHeight);
			remWidth -= wW;
			remX += wW;
		}
		if(eastComposite != null){
			int eW = (eastWidth == SWT.DEFAULT) 
				? eastComposite.computeSize(SWT.DEFAULT, height).x
				: eastWidth;			
			eastComposite.setBounds(x + width - eW, remY, eW, remHeight);
			remWidth -= eW;
		}
		if(centerComposite != null){
			centerComposite.setBounds(remX, remY, remWidth, remHeight);
		}
	}
	
}
