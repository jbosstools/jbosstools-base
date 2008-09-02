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
package org.jboss.tools.common.resref.core;

import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.model.ui.objecteditor.XTableImageProvider;
import org.jboss.tools.common.model.ui.objecteditor.XTableProvider;

public class ResourceReferencesTableProvider implements XTableProvider, XTableImageProvider {
	static String[] CSS_COLUMNS = new String[]{"Scope", "CSS File Path"};
	static String[] IMG_COLUMNS = new String[]{"Scope", "Image Folder Path"};
	static String[] TLD_COLUMNS = new String[]{"Scope", "URI", "Prefix"};
	static String[] EL_COLUMNS = new String[]{"Scope", "El Expression", "Value"};
	private final static String[] GLOBAL_EL_COLUMNS = new String[]{"Scope","El Expression", "Value"};
	
	int[] widths = new int[]{50, 200};
	List dataList;
	String[] columns;
	
	public static ResourceReferencesTableProvider getCSSTableProvider(List dataList) {
		ResourceReferencesTableProvider p = new ResourceReferencesTableProvider(dataList);
		p.columns = CSS_COLUMNS;
		return p;
	}

	public static ResourceReferencesTableProvider getImageTableProvider(List dataList) {
		ResourceReferencesTableProvider p = new ResourceReferencesTableProvider(dataList);
		p.columns = IMG_COLUMNS;
		return p;
	}

	public static ResourceReferencesTableProvider getTLDTableProvider(List dataList) {
		ResourceReferencesTableProvider p = new ResourceReferencesTableProvider(dataList);
		p.columns = TLD_COLUMNS;
		p.widths = new int[]{50, 150, 50};
		return p;
	}
	
	   public static ResourceReferencesTableProvider getELTableProvider(List dataList) {
	        ResourceReferencesTableProvider p = new ResourceReferencesTableProvider(dataList);
	        p.columns = EL_COLUMNS;
	        p.widths = new int[]{50, 150, 50};
	        return p;
	    }
	   
	    
       public static ResourceReferencesTableProvider getGlobalELTableProvider(final List dataList) {
            ResourceReferencesTableProvider p = new ResourceReferencesTableProvider(dataList);
            
            p.columns = GLOBAL_EL_COLUMNS;
            p.widths = new int[]{50,150, 50};
            return p;
        }
	    
	
	

	private ResourceReferencesTableProvider(List dataList) {
		this.dataList = dataList;
	}

	public int getColumnCount() {
		return columns.length;
	}

	public int getRowCount() {
		if(dataList == null) return 0;
		return dataList.size();
	}

	public String getColumnName(int c) {
		return columns[c];
	}

	public String getValueAt(int r, int c) {
		ResourceReference css = (ResourceReference)dataList.get(r);
		return (c == 0) ? css.getScopeName() : (c == 2) ? css.getProperties() : css.getLocation();
	}

	public Object getDataAt(int r) {
		return null;
	}

	public Color getColor(int r) {
		return null;
	}

	public int getWidthHint(int c) {
		return widths[c];
	}

	public void dispose() {
	}

	public Image getImage(int r) {
		return null;
	}

}
