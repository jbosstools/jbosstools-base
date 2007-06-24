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
package org.jboss.tools.common.model.undo;

import org.eclipse.swt.graphics.Image;

public class XUndoItem {
    private Image[] icons = null;
    private String description = "";

    public XUndoItem(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public Image[] getIcons() {
        return icons;
    }

    void setIcons(Image[] icons) {
        this.icons = icons;
    }
}
