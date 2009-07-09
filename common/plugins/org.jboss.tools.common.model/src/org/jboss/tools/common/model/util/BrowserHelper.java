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
package org.jboss.tools.common.model.util;

import java.io.*;
import java.text.MessageFormat;
import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.engines.impl.XProcess;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.plugin.ModelMessages;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.meta.action.impl.handlers.*;

public class BrowserHelper {
	static String ATTRIBUTE = "Browser Path"; //$NON-NLS-1$
	

    public BrowserHelper() {}

	public static void startExplorer(XModel model, String url) throws XModelException {
		XModelObject editor = PreferenceModelUtilities.getPreferenceModel().getByPath("%Options%/Struts Studio/Running"); //$NON-NLS-1$
		if(editor == null) throw new XModelException(ModelMessages.BrowserHelper_InternetBrowserNotSet);
		if(validatePath(PreferenceModelUtilities.getPreferenceModel().getService(), editor)) new OWEProcess(editor, url).start();
	}
	
	static boolean validatePath(ServiceDialog d, XModelObject o) {
		String[] paths = getEnvironmentPaths();
		String en = o.getModelEntity().getName();
		XEntityData[] dt = new XEntityData[]{XEntityDataImpl.create(new String[][]{{en, XModelObjectConstants.YES}, {ATTRIBUTE, XModelObjectConstants.YES}})};
		String path = o.getAttributeValue(ATTRIBUTE).replace('\\','/');
		XAttributeData ad = HUtil.find(dt, 0, ATTRIBUTE);
		ad.setValue(path);
		while(true) {
			String b = ad.getValue();
			if(b != null && b.length() > 0)
			  try {
				  if(fileExists(b, paths)) {
					  if(!b.equals(path)) {
						  o.getModel().changeObjectAttribute(o, ATTRIBUTE, b);
						o.getModel().saveOptions();
					  }
					  return true;
				  }
			  } catch (XModelException e) {
				  ModelPlugin.getPluginLog().logError("BrowserHelper:" + e.getMessage()); //$NON-NLS-1$
			  }
			int i = d.showDialog(ModelMessages.BrowserHelper_DialogTitleRun, MessageFormat.format(ModelMessages.BrowserHelper_EnterValidPath, o.getPresentationString()),
								 new String[]{ModelMessages.OK, ModelMessages.Cancel}, dt[0], ServiceDialog.QUESTION);
			if(i != 0) return false;
		}
	}

	static String[] getEnvironmentPaths() {
			String jlp = OSHelper.getProperty("PATH", ""); //$NON-NLS-1$ //$NON-NLS-2$
			StringTokenizer st = new StringTokenizer(jlp, File.pathSeparator);
			String[] ps = new String[st.countTokens()];
			for (int i = 0; i < ps.length; i++) ps[i] = st.nextToken();
			return ps;
	}

	static boolean fileExists(String filename, String[] paths) {
		filename = filename.replace('\\', '/');
		if(paths == null || filename.indexOf('/') >= 0) return new File(filename).isFile();
		for (int i = 0; i < paths.length; i++) {
			String f = paths[i] + XModelObjectConstants.SEPARATOR + filename;
			if(new File(f).isFile()) return true;
		}
		return false;
	}

}

class OWEProcess extends XProcess {
	private XModelObject o;
	private String url;

	public OWEProcess(XModelObject o, String url) {
		this.o = o;
		this.url = url;
	}

	protected String getRoot() {
		return "."; //$NON-NLS-1$
	}

	protected void write(String s) {
		o.getModel().getOut().print(s);
	}

	protected void buildCommandLine(ArrayList<String> l) {
		String program = o.getAttributeValue(BrowserHelper.ATTRIBUTE);
		l.add(program);
		l.add(url);
	}

}
