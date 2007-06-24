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
package org.jboss.tools.common.model.ui.texteditors;

import java.util.Properties;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.util.*;

public class TextEditorSupport {
	protected XModelObjectCache object = null;
	protected int lock = 0;
	private boolean modified = false;
	protected ITextProvider provider;
	protected long timeStamp = -1;
	
	public void setProvider(ITextProvider provider) {
		this.provider = provider;
	}

	public void setObject(XModelObject object) {
		this.object = new XModelObjectCache(object);
		update();
	}
	
	public XModelObject getModelObject() {
		return (object == null) ? null : object.getObject();
	}

	public void update() {
		if(lock > 0) return;
		if(!needsUpdate()) return;
		lock++;
		try {
			String content = loadContent();		
			if(provider.isEqualText(content)) {
				return;
			}
			provider.setText(content);
			setModified(false);
		} finally {
			lock--;
		}
	}

	protected String loadContent() {
		XModelObject o = getModelObject();
		try {
			return ((FileAnyImpl)o).getAsText();
		} catch (Exception e) {
			return "";
		}
	}

	public void setModified(boolean set) {
		if(modified == set) return;
		modified = set;
		XModelObject o = getModelObject();
		if(o != null && set) o.setModified(true);
	}
	
	public boolean isModified() {
		return modified;
	}
	
	public boolean canSave(boolean force) {
		return (lock == 0 && (modified || force));
	}

	public void save() {
		save(false);
	}
	
	public void save(boolean force) {
		if(!canSave(force)) return;
		lock++;
		try {
			FileAnyImpl f = (FileAnyImpl)getModelObject();
			if(f != null) f.edit(provider.getText()); 
		} catch (Exception e) {
			ModelUIPlugin.log(e);
		} finally {
			lock--;
			modified = false;
		}
	}
	
	protected boolean needsUpdate() {
		XModelObject o = (object == null) ? null : object.getObject();
		if(o == null) return false;
		long ts = o.getTimeStamp();
		if(timeStamp == ts) return false;
		timeStamp = ts;
//		String entity = o.getModelEntity().getName();
//		if("FileAnyLong".equals(entity)) {
//			return false;
//		}
		return true;
	}
	
	public boolean canRevertToSaved() {
		XModelObject o = getModelObject();
		if(o == null || !o.isActive()) return false;
		XAction a = o.getModelEntity().getActionList().getAction("DiscardActions.Discard");
		return (a != null && a.isEnabled(o));		
	}
	
	public void revertToSaved() {
		XModelObject o = getModelObject();
		if(o != null) {
			XActionInvoker.invoke("DiscardActions.Discard", o, new Properties());
		}
	}

}
