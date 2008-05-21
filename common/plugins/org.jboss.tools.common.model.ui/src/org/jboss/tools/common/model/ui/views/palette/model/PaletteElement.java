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

import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.XModelObject;

public class PaletteElement implements IPaletteNode {
	private XModelObject xobject;
	private String title;
	private Image image;
	private String description;
	private String startText;
	private String endText;
	private boolean reformat;
	
	public PaletteElement(XModelObject xobject,
						String title,
						Image image,
						String description,
						String startText,
						String endText,
						boolean reformat) {
		this.xobject = xobject;
		this.title = title;
		this.image = image;
		this.description = description;
		this.startText = startText;
		this.endText = endText;
		this.reformat = reformat;
	}

	public String getTitle() {
		return title;
	}

	public String getDescription() {
		return description;
	}

	public String getStartText() {
		return startText;
	}

	public String getEndText() {
		return endText;
	}

	public Image getImage() {
		return image;
	}
	
	public boolean getReformat() {
		return reformat;
	}
	
	public IPaletteNode[] getChildren() {
		return null;
	}

	public XModelObject getXModelObject() {
		return xobject;
	}

	public void setImage(Image image) {
		this.image = image;
	}
	
	public IPaletteNode addChild(IPaletteNode node) {
		return node;
	}
}
