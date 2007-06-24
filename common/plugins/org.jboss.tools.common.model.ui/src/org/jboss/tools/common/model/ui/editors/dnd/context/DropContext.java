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
package org.jboss.tools.common.model.ui.editors.dnd.context;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.jboss.tools.common.model.ui.dnd.ModelTransfer;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.HTMLTransfer;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.wst.sse.ui.internal.TextDropAction;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.ui.editors.dnd.FileTagProposalLoader;
import org.jboss.tools.jst.web.tld.TLDUtil;

/**
 * @author glory
 */

public class DropContext {
	private boolean isOverAttributeValue = false;
	private String attributeName = null;
	private String flavor = null;
	private String mimeData = null;
	private DropTargetEvent event;
	
	public void clean() {
		isOverAttributeValue = false;
		flavor = null;
		mimeData = null;
		attributeName = null;
	}
	
	public boolean isOverAttributeValue() {
		return isOverAttributeValue;
	}
	
	public void setOverAttributeValue(boolean b) {
		isOverAttributeValue = b;
	}
	
	public void setAttributeName(String attr) {
		this.attributeName = attr;
	}
	
	public String getAttributeName() {
		return attributeName;
	}
	
	public String getFlavor() {
		return flavor;
	}
	
	public String getMimeData() {
		return mimeData;
	}
	
	TransferHolder[] TRANSFERS = {
		new TransferHolder(ModelTransfer.getInstance(), new ModelTransferProcessor()),
		new TransferHolder(HTMLTransfer.getInstance(), new HTMLTransferProcessor()),
		new TransferHolder(FileTransfer.getInstance(), new FileTransferProcessor()),
		new TransferHolder(TextTransfer.getInstance(), new TextTransferProcessor()),
	};

	public void setDropTargetEvent(DropTargetEvent event) {
		this.event = event;
		TransferData[] ds = event.dataTypes;
		for (int i = 0; i < TRANSFERS.length; i++) {
			TransferData data = TRANSFERS[i].getSupportedData(ds);
			if(data != null) {
				TRANSFERS[i].process(data);
				break;
			}			
		}
	}

	private String getURL(DropTargetEvent event) {
		String[] s = (String[])event.data;
		if(s == null || s.length == 0) return null;
		File f = new File(s[0]);
		try {
			return f.toURL().toString();
		} catch (Exception e) {
			return s[0];
		}
	}
	private File getFile(DropTargetEvent event) {
		String[] s = (String[])event.data;
		return(s == null || s.length == 0) ? null : new File(s[0]);
	}
	
	private String getURL(XModelObject o) {
		if(o.getFileType() == XModelObject.FILE) {
			IFile f = (IFile)EclipseResourceUtil.getResource(o);
			try {
				return f.getLocation().toFile().toURL().toString();
			} catch (Exception e) {}
		}
		return null;
	}

	public void runDropCommand(IDNDTextEditor editor, DropTargetEvent event) {
		setDropTargetEvent(event);
		runDropCommand(editor);
	}

	public void runDropCommand(IDNDTextEditor editor) {
		if(event == null) return;
		if(getFlavor() == null) {
		} else if (getFlavor().equals("text/html")) {
			new TextDropAction().run(event, editor);
		} else {
			editor.runDropCommand(getFlavor(), getMimeData());
		}
		event = null;
	}
	
	private boolean dropAsFileObject(XModelObject o) {
		if(o.getFileType() != XModelObject.FILE || isOverAttributeValue) return false;
    	if(TLDUtil.isTaglib(o)) return false;
    	String extension = o.getAttributeValue("extension");
    	return extension != null && FileTagProposalLoader.isExtensionMapped(extension);
	}
	
	class TransferHolder {
		Transfer transfer;
		TransferProcessor processor;
		public TransferHolder(Transfer transfer, TransferProcessor processor) {
			this.transfer = transfer;
			this.processor = processor;
		}
		public TransferData getSupportedData(TransferData[] data) {
			
			for (int i = 0; i < data.length; i++) {
				if(transfer.isSupportedType(data[i])) return data[i];
			}
			return null;
		}
		public void process(TransferData data) {
			processor.process(data);
		}
	}
	
	abstract class TransferProcessor {
		abstract void process(TransferData data);
	}
	
	class ModelTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			XModelObject o = PreferenceModelUtilities.getPreferenceModel().getModelBuffer().source();
			if(dropAsFileObject(o)) {
				flavor = "application/x-moz-file";
				mimeData = getURL(o);
				if(mimeData == null) flavor = ModelTransfer.MODEL;
			} else {
				flavor = ModelTransfer.MODEL;
			}
		}
	}

	class HTMLTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			flavor = "text/html";
			Object ooo = HTMLTransfer.getInstance().nativeToJava(event.currentDataType);
			mimeData = ooo == null ? null : ooo.toString();
		}
	}

	class FileTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			flavor = "application/x-moz-file";
			mimeData = getURL(event);
			if(mimeData != null && isOverAttributeValue) {
				String path = getFile(event).getAbsolutePath();
				IFile f = EclipseResourceUtil.getFile(path);
				XModelObject fo = (f == null) ? null : EclipseResourceUtil.getObjectByResource(f);
				if(fo != null) {
					fo.getModel().getModelBuffer().clear();
					fo.getModel().getModelBuffer().addSource(fo);
					flavor = ModelTransfer.MODEL;
				}
			}
		}
	}

	class TextTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			flavor = "text/html";
		}
	}

}
