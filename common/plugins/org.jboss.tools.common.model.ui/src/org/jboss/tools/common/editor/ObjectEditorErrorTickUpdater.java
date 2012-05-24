/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.viewsupport.IProblemChangedListener;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.internal.ui.viewsupport.JavaUILabelProvider;
import org.eclipse.jdt.ui.ProblemsLabelDecorator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;

public class ObjectEditorErrorTickUpdater implements IProblemChangedListener {

	private ObjectMultiPageEditor fJavaEditor;
	private JavaUILabelProvider fLabelProvider;

	public ObjectEditorErrorTickUpdater(ObjectMultiPageEditor editor) {
		Assert.isNotNull(editor);
		fJavaEditor= editor;
		fLabelProvider=  new JavaUILabelProvider(0, JavaElementImageProvider.SMALL_ICONS);
		fLabelProvider.addLabelDecorator(new ProblemsLabelDecorator(null));
		JavaPlugin.getDefault().getProblemMarkerManager().addListener(this);
	}

	/*
	 * @see IProblemChangedListener#problemsChanged(IResource[], boolean)
	 */
	public void problemsChanged(IResource[] changedResources, boolean isMarkerChange) {
		if (!isMarkerChange)
			return;

		IEditorInput input= fJavaEditor.getEditorInput();
		if (input instanceof IFileEditorInput) {
			IFile resource = ((IFileEditorInput)input).getFile();
			if (resource != null) {
				for (int i = 0; i < changedResources.length; i++) {
					if (changedResources[i].equals(resource)) {
						updateEditorImage(resource);
					}
				}
			}
		}
	}

	public void updateEditorImage(IFile jelement) {
		Image titleImage= fJavaEditor.getTitleImage();
		if (titleImage == null) {
			return;
		}
		Image newImage = fLabelProvider.getImage(jelement);
		if (titleImage != newImage) {
			postImageChange(newImage);
		}
	}

	private void postImageChange(final Image newImage) {
		Shell shell= fJavaEditor.getEditorSite().getShell();
		if (shell != null && !shell.isDisposed()) {
			shell.getDisplay().syncExec(new Runnable() {
				public void run() {
					fJavaEditor.updatedTitleImage(newImage);
				}
			});
		}
	}

	public void dispose() {
		fLabelProvider.dispose();
		JavaPlugin.getDefault().getProblemMarkerManager().removeListener(this);
	}
}
