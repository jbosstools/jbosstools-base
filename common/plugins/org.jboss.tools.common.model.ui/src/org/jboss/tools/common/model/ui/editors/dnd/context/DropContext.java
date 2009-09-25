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
import java.net.MalformedURLException;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.HTMLTransfer;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.events.TypedEvent;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelTransferBuffer;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.dnd.ModelTransfer;
import org.jboss.tools.common.model.ui.dnd.VpeDnDEvent;
import org.jboss.tools.common.model.ui.dnd.VpeTextDropAction;
import org.jboss.tools.common.model.ui.editors.dnd.DropUtils;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

/**
 * @author glory
 */

public class DropContext {
	private boolean isOverAttributeValue = false;
	private String attributeName = null;
	private String flavor = null;
	private String mimeData = null;
	private TypedEvent event;
	
	public void setFlavor(String flavor) {
        this.flavor = flavor;
    }

    public void setMimeData(String mimeData) {
        this.mimeData = mimeData;
    }

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
		new TransferHolder(LocalSelectionTransfer.getTransfer(), new LocalTransferProcessor()),
		new TransferHolder(ModelTransfer.getInstance(), new ModelTransferProcessor()),
		new TransferHolder(HTMLTransfer.getInstance(), new HTMLTransferProcessor()),
		new TransferHolder(FileTransfer.getInstance(), new FileTransferProcessor()),
		new TransferHolder(TextTransfer.getInstance(), new TextTransferProcessor()),
	};

	public void setDropTargetEvent(TypedEvent event) {
		this.event = event;
	
		TransferData[] ds = null;
		if(this.event instanceof DropTargetEvent){
		    ds = ((DropTargetEvent)event).dataTypes;
		}else if(this.event instanceof VpeDnDEvent){
		    ds = ((VpeDnDEvent)this.event).getDataTypes();
		}
		if(ds!=null){
		for (int i = 0; i < TRANSFERS.length; i++) {
			TransferData data = TRANSFERS[i].getSupportedData(ds);
			if(data != null) {
				TRANSFERS[i].process(data);
				break;
			}		
			
		}
		}
	}

	private String getURL(TypedEvent event) {
		String[] s = (String[])event.data;
		if(s == null || s.length == 0) {
			return null;
		} else {
			return DropUtils.convertPathToUrl(s[0]);
		}
	}
	private File getFile(TypedEvent event) {
		String[] s = (String[])event.data;
		return(s == null || s.length == 0) ? null : new File(s[0]);
	}
	
	private String getURL(XModelObject o) {
		if(o.getFileType() == XModelObject.FILE) {
			IFile f = (IFile)EclipseResourceUtil.getResource(o);
			if(f == null || !f.exists()) return null;
			try {
				return f.getLocation().toFile().toURL().toString();
			} catch (MalformedURLException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
		}
		return null;
	}

	public void runDropCommand(IDNDTextEditor editor, TypedEvent event) {
		setDropTargetEvent(event);
		runDropCommand(editor);
	}

	public void runDropCommand(IDNDTextEditor editor) {
		if(event == null) return;
		if(getFlavor() == null) {
		    return;
		} else if (getFlavor().equals("text/html")) { //$NON-NLS-1$
		    new VpeTextDropAction().run(event, editor);
		} else {
			editor.runDropCommand(getFlavor(), getMimeData());
		}
		event = null;
	}

	//see FileTagProposalLoader
	public static Set<String> mappedExtensions = new HashSet<String>();
	public static boolean isExtensionMapped(String extension) {
		return extension != null && mappedExtensions.contains(extension.toLowerCase());
	}
	
	private boolean dropAsFileObject(XModelObject o) {
		if(o == null) return false;
		if(o.getFileType() != XModelObject.FILE || isOverAttributeValue) return false;
    	if(isTaglib(o)) return false;
    	String extension = o.getAttributeValue("extension"); //$NON-NLS-1$
    	return extension != null && isExtensionMapped(extension);
	}

	//TODO do not reference tld here
	//TLDUtil.isTaglib(o)
    static String FILE_ENTITIES = ".FileTLD_PRO.FileTLD_1_2.FileTLD_2_0.FileTLD_2_1."; //$NON-NLS-1$
    public static boolean isTaglib(XModelObject o) {
        return isOfEntity(o, FILE_ENTITIES);
    }
    private static boolean isOfEntity(XModelObject o, String entities) {
        return entities.indexOf("." + o.getModelEntity().getName() + ".") >= 0;  //$NON-NLS-1$//$NON-NLS-2$
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
			if(!XModelTransferBuffer.getInstance().isEnabled()) return;
			XModelObject o = PreferenceModelUtilities.getPreferenceModel().getModelBuffer().source();
			if(dropAsFileObject(o)) {
				flavor = "application/x-moz-file"; //$NON-NLS-1$
				mimeData = getURL(o);
				if(mimeData == null) flavor = ModelTransfer.MODEL;
			} else {
				flavor = ModelTransfer.MODEL;
			}
		}
	}

	class HTMLTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			flavor = "text/html"; //$NON-NLS-1$
			TransferData currentDataType = null;
			if(event instanceof DropTargetEvent){
			    currentDataType = (( DropTargetEvent)event).currentDataType;
			}else if(event instanceof VpeDnDEvent){
			    currentDataType = ((VpeDnDEvent)event).currentDataType;
			}
			Object ooo = HTMLTransfer.getInstance().nativeToJava(currentDataType);
		
			mimeData = ooo == null ? null : ooo.toString();
			if(mimeData == null) {
				flavor = null;
			}
		}
	}

	class LocalTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			flavor = "application/x-moz-file"; //$NON-NLS-1$
			Object ooo = LocalSelectionTransfer.getTransfer().getSelection();
			if(ooo instanceof StructuredSelection) {
				ooo = ((StructuredSelection)ooo).getFirstElement();
				InnerDragBuffer.object = ooo;
			}
			mimeData = ooo == null ? null : ooo.toString();
		}
	}

	class FileTransferProcessor extends TransferProcessor {
		public void process(TransferData data) {
			ISelection s = LocalSelectionTransfer.getTransfer().getSelection();
			if(s != null) {
				if(s.isEmpty() || !(s instanceof IStructuredSelection)) return;
				Object o = ((IStructuredSelection)s).getFirstElement();
				boolean ok = o instanceof IFile;
				if(!ok && (o instanceof IAdaptable)) {
					ok = ((IAdaptable)o).getAdapter(IFile.class) != null
						&& ((IAdaptable)o).getAdapter(IResource.class) != null;
				}
				if(!ok) return;
			}			
			mimeData = getURL(event);
			flavor = "application/x-moz-file"; //$NON-NLS-1$
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
			flavor = "text/html"; //$NON-NLS-1$
		}
	}

}
