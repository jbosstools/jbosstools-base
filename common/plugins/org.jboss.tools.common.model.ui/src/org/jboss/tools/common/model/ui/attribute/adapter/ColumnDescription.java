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
package org.jboss.tools.common.model.ui.attribute.adapter;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;

/**
 * @author au
 */
public class ColumnDescription {
    private static final String DEFAULT_COLUMN_NAME = "Name"; 
    private static final int DEFAULT_COLUMN_WIDTH = 10; 
    private static final int DEFAULT_COLUMN_STYLE = SWT.LEFT;
    private static final boolean DEFAULT_COLUMN_RESIZEABLE = true;
    
    private String name;
    private Image image;
    private int width;
    private int style;
    private boolean resizeable;
    private Object data;
    
    public ColumnDescription() {
        name = DEFAULT_COLUMN_NAME;
        image = null;
        width = DEFAULT_COLUMN_WIDTH;
        style = DEFAULT_COLUMN_STYLE;
        resizeable = DEFAULT_COLUMN_RESIZEABLE;
        data = null;
    }
    
    public ColumnDescription(String name, Image image, int width, int style, boolean resizeable, Object data) {
        this.name = name;
        this.image = image;
        this.width = width;
        this.style = style;
        this.resizeable = resizeable;
        this.data = data;
    }
    
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public boolean isResizeable() {
        return resizeable;
    }
    public void setResizeable(boolean resizeable) {
        this.resizeable = resizeable;
    }
    public int getStyle() {
        return style;
    }
    public void setStyle(int style) {
        this.style = style;
    }
    public int getWidth() {
        return width;
    }
    public void setWidth(int width) {
        this.width = width;
    }
    public Object getData() {
        return data;
    }
    public void setData(Object data) {
        this.data = data;
    }
    public Image getImage() {
        return image;
    }
    public void setImage(Image image) {
        this.image = image;
    }
}
