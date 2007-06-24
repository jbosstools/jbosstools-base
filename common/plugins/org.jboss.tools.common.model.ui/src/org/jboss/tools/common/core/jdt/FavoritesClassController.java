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
package org.jboss.tools.common.core.jdt;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class FavoritesClassController {

	private static final String FAVORITES_CLASSES_LIST 	= "JavaFavoritesFieldEditor.classes";
	private static final String DEFAULT_CLASSES_LIST 	= "FavoritesClassController.defaultClasses";
	private static final Image CLASS_ICON				= JavaPluginImages.get(JavaPluginImages.IMG_OBJS_CLASS);


	public static List<String> getFavoritesClassesList() {
		ArrayList<String> list = new ArrayList<String>();
		String classesList = ModelUIPlugin.getDefault().getPreferenceStore().getString(FAVORITES_CLASSES_LIST);
		if ((classesList==null)||(classesList.length()==0)) {
			classesList = Messages.getString(DEFAULT_CLASSES_LIST);
		}
		StringTokenizer st = new StringTokenizer(classesList,",");
		while (st.hasMoreTokens()) {
			 list.add(st.nextToken());
		}
		return list;
	}

	private static String getClassesListString(Collection list) {
		StringBuffer buffer = new StringBuffer();
		Iterator i = list.iterator();
		while (i.hasNext()) {
			buffer.append((String)i.next()+",");
		}
		return buffer.toString();
	}
	
	public static void push(String className) {
		List<String> list = getFavoritesClassesList();
		int index = -1;
		for (int i=0;i<list.size();++i) {
			if (className.equals((String)list.get(i))) {
				index = i;
				break;
			}
		}
		if (index>-1) {
			list.add(0, list.get(index));
			list.remove(index+1);
		} else {
			list.add(0,className);
		}
		ArrayList<String> newList = new ArrayList<String>();
		if (list.size() > 10) {
			for (int i = 0; i < 10; ++i) newList.add(list.get(i));
			list = newList;
		}
		ModelUIPlugin.getDefault().getPreferenceStore().setValue(FAVORITES_CLASSES_LIST, getClassesListString(list));
	}
	
	private static LabelProvider labelProvider = new FavoritesClassLabelProvider();
	
	public static LabelProvider getLabelProvider() {
		return labelProvider;
	}
	
	static class FavoritesClassLabelProvider extends LabelProvider {
		public Image getImage(Object element) {
			return CLASS_ICON;
		}
	}
}
