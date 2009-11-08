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
package org.jboss.tools.common.model.ui.wizards.query;

import java.util.Properties;
import org.eclipse.jface.dialogs.*;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.jboss.tools.common.model.ui.action.CommandBarListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.meta.help.HelpUtil;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.ui.*;

public abstract class AbstractQueryWizardView implements CommandBarListener {
	public static String CANCEL = ModelUIMessages.AbstractQueryWizardView_Cancel;
	public static String CLOSE = ModelUIMessages.AbstractQueryWizardView_Close;
	public static String OK = ModelUIMessages.AbstractQueryWizardView_OK;	
	public static String HELP = ModelUIMessages.AbstractQueryWizardView_Help;	
	private String helpkey = null;	
	private Dialog dialog; 
	private TitleAreaDialog titleDialog = null;
	private int code = 1;
	protected CommandBar commandBar = new CommandBar();	
	protected XModel model;

	protected String windowTitle;
	private String title = ""; //$NON-NLS-1$
	private String message;
	private String errorMessage;
	private Image titleImage;
	
	public AbstractQueryWizardView() {}
	
	public void setModel(XModel model) {
		this.model = model;
	}
	
	public CommandBar getCommandBar() {
		return commandBar;
	}
	
	public String[] getCommands() {
		return new String[]{OK, CANCEL};
	}
	
	public String getDefaultCommand() {
		return OK;
	}

	public abstract Control createControl(Composite parent);
	
	public void setObject(Object object) {
		Properties p = findProperties(object);
		if(p != null) {
			String key = p.getProperty("help"); //$NON-NLS-1$
			setHelpKey(key);
			windowTitle = WizardKeys.getHeader(key);
			title = WizardKeys.getTitle(key);
			if(title == null) title = p.getProperty("title", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	protected Properties findProperties(Object o) {
		if(o instanceof Properties) return (Properties)o;
		if(!(o instanceof Object[])) return null;
		Object[] os = (Object[])o;
		for (int i = 0; i < os.length; i++)
		  if(os[i] instanceof Properties) return (Properties)os[i];
		return null;
	}

	public void dispose() {
		if (titleDialog!=null) titleDialog.close();
		titleDialog = null;
		if (dialog!=null) dialog.close();
		dialog = null;
		if (commandBar!=null) commandBar.dispose();
		commandBar = null;
	}

	public void stopEditing() {}
	
	public void action(String command) {
		stopEditing();
		if(CANCEL.equals(command)) {
			code = 1;
			dispose();
		} else if(OK.equalsIgnoreCase(command)) {
			code = 0;
			dispose();
		} else if(HELP.equals(command)) {
			HelpUtil.helpEclipse(model, helpkey);
		}
	}

	public int code() {
		return code;
	}

	public void exception(Exception e) {
		ModelUIPlugin.getPluginLog().logError(e);
	}

	public void setHelpKey(String key) {
		helpkey = key;
	}
	
	public String getHelpKey() {
		return helpkey;
	}

	public void setErrorMessage(String string) {
		errorMessage = string;
		if (titleDialog != null) {
			titleDialog.setErrorMessage(errorMessage);
		}
	}
	public void setMessage(String string) {
		message = string;
		if (titleDialog != null) {
			titleDialog.setMessage(message);
		}
	}
	public void setTitle(String string) {
		title = string;
		if ((title!=null)&&(titleDialog!=null)) {
			titleDialog.setTitle(title); 
		}
	}
	public void setWindowTitle(String string) {
		windowTitle = string;
		if ((windowTitle!=null)&&(dialog!=null)) {
			dialog.getShell().setText(windowTitle);
		}
	}
	public void setTitleImage(Image image) {
		titleImage = image;
		if ((titleImage!=null)&&(titleDialog!=null)) {
			titleDialog.setTitleImage(titleImage);
		} else {
			titleDialog.setTitleImage(ModelUIImages.getImageDescriptor(ModelUIImages.WIZARD_DEFAULT).createImage(null));
		}
	}

/*
	public AbstractQueryDialog getDialog() {
		return dialog;
	}
*/
	public void setDialog(Dialog d) {
		dialog = d;
		titleDialog = (dialog instanceof TitleAreaDialog) ? (TitleAreaDialog)dialog : null;
		code = 1;
		if (windowTitle!=null) {
			dialog.getShell().setText(windowTitle);
		}
		if(titleDialog != null) {
			if (title!=null) {
				titleDialog.setTitle(title); 
			}
			if (message!=null) {
				titleDialog.setMessage(message);
			}
			if (errorMessage!=null) {
				titleDialog.setErrorMessage(errorMessage);
			}
			if (titleImage!=null) {
				titleDialog.setTitleImage(titleImage);
			}
		}
	}
	
	public Point getPreferredSize() {
		return null;
	}
	
	public void setCode(int code) {
		this.code = code;
	}
	
	public void updateBar() {
		
	}
}
