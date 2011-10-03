/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.AggregateValidationStatus;
import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.observable.IObservableCollection;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.fieldassist.ControlDecorationSupport;
import org.eclipse.jface.databinding.swt.WidgetProperties;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.ui.CommonUIMessages;

/**
 * @author André Dietisheim
 */
public class DataBindingUtils {

	/**
	 * Binds the enabled status of the given button to the validity status of
	 * the given providers. If those are not provided the validation status of
	 * the whole context is used.
	 * 
	 * @param button
	 *            the button whose enablement is bound
	 * @param validationStatusProviders
	 *            the providers that shall toggle the button enablement
	 * @param dbc
	 *            the data binding context to use when binding
	 */
	public static void bindButtonEnablementToValidationStatus(final Button button,
			IObservableCollection validationStatusProviders, DataBindingContext dbc) {
		dbc.bindValue(
				WidgetProperties.enabled().observe(button),
				createAggregateValidationStatus(validationStatusProviders, dbc),
				new UpdateValueStrategy(UpdateValueStrategy.POLICY_NEVER),
				new UpdateValueStrategy().setConverter(new Status2BooleanConverter(IStatus.ERROR)));
	}

	/**
	 * Creates an aggregated validation status for the given providers and
	 * databinding context. If no providers are given the whole context is used
	 * a provider.
	 * 
	 * @param validationStatusProviders
	 *            to use
	 * @param dbc
	 *            the data binding context to use
	 * @return
	 */
	protected static AggregateValidationStatus createAggregateValidationStatus(
			IObservableCollection validationStatusProviders, DataBindingContext dbc) {
		AggregateValidationStatus aggregatedValidationStatus;
		if (validationStatusProviders == null) {
			aggregatedValidationStatus = new AggregateValidationStatus(dbc, AggregateValidationStatus.MAX_SEVERITY);
		} else {
			aggregatedValidationStatus = new AggregateValidationStatus(
					validationStatusProviders, AggregateValidationStatus.MAX_SEVERITY);
		}
		return aggregatedValidationStatus;
	}

	/**
	 * Returns an observable collection for a given array of observable values.
	 * 
	 * @param observableValue
	 *            the array of observable values
	 * @return an observable collection
	 */
	public static IObservableCollection toObservableCollection(IObservableValue... observableValues) {
		WritableList observableCollection = new WritableList();
		for (IObservableValue observableValue : observableValues) {
			observableCollection.add(observableValue);
		}
		return observableCollection;
	}

	/**
	 * Binds the given text field with the given name to the given model
	 * property. A validator, that wont validate if there's no content in the
	 * text field is attached.
	 * 
	 * @param text
	 *            the text field to bind
	 * @param fieldName
	 *            the name of the text field
	 * @param modelProperty
	 *            the property of the model to bind to
	 * @param model
	 *            the model to bind to
	 * @param dbc
	 *            the data binding context to use
	 * @return
	 */
	public static Binding bindMandatoryTextField(Text text, String fieldName, String modelProperty,
			Object model, DataBindingContext dbc) {
		Binding binding = dbc.bindValue(
				WidgetProperties.text(SWT.Modify).observe(text),
				BeanProperties.value(modelProperty).observe(model),
				new UpdateValueStrategy().setAfterGetValidator(
						new MandatoryStringValidator(NLS.bind(
								CommonUIMessages.MANDATORYSTRING_VALIDATOR_MUST_PROVIDE_VALUE, fieldName))),
				null);
		ControlDecorationSupport.create(binding, SWT.LEFT | SWT.TOP);
		return binding;
	}

}
