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
package org.jboss.tools.common.model.ui.views.palette.model;

import java.util.ArrayList;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.XModelObject;

public class PaletteGroup implements IPaletteNode {
	private XModelObject xobject;
	private String title;
	private Image image;
	private ArrayList<IPaletteNode> elems = new ArrayList<IPaletteNode>();
	
	public PaletteGroup(XModelObject xobject) {
		this(xobject, null, null);
	}
	
	public PaletteGroup(XModelObject xobject, String title) {
		this(xobject, title, null);
	}
	
	public PaletteGroup(XModelObject xobject, String title, Image image) {
		this.xobject = xobject;
		this.title = title;
		this.image = image;
	}

	public String getTitle() {
		return title;
	}

	public String getDescription() {
		return null;
	}

	public Image getImage() {
		return image;
	}
	
	public IPaletteNode[] getChildren() {
		if (elems.size() > 0) {
			return (IPaletteNode[])elems.toArray(new IPaletteNode[elems.size()]);
		} else {
			return null;
		}
	}

	public XModelObject getXModelObject() {
		return xobject;
	}

	public void setImage(Image image) {
		this.image = image;
	}

	public IPaletteNode addChild(IPaletteNode node) {
		elems.add(node);
		return node;
	}

	public void clear() {
		elems.clear();
	}
}
