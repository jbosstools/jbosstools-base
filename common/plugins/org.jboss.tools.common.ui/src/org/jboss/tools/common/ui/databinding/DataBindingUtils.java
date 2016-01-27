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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.databinding.AggregateValidationStatus;
import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.ValidationStatusProvider;
import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.observable.IObservable;
import org.eclipse.core.databinding.observable.IObservableCollection;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.observable.value.IValueChangeListener;
import org.eclipse.core.databinding.observable.value.ValueChangeEvent;
import org.eclipse.core.databinding.validation.MultiValidator;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.fieldassist.ControlDecorationSupport;
import org.eclipse.jface.databinding.swt.WidgetProperties;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.jboss.tools.common.databinding.MandatoryStringValidator;
import org.jboss.tools.common.databinding.StatusSeverity2BooleanConverter;
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
	public static void bindEnablementToValidationStatus(final Control control,
			DataBindingContext dbc, Binding... bindings) {
		bindEnablementToValidationStatus(control, IStatus.OK, dbc, bindings);
	}

	public static void bindEnablementToValidationStatus(final Control control, int enabledSeverityMask,
			DataBindingContext dbc, Binding... bindings) {
		dbc.bindValue(
				WidgetProperties.enabled().observe(control),
				createAggregateValidationStatus(dbc, bindings),
				new UpdateValueStrategy(UpdateValueStrategy.POLICY_NEVER),
				new UpdateValueStrategy().setConverter(new StatusSeverity2BooleanConverter(enabledSeverityMask)));
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
			DataBindingContext dbc, Binding... bindings) {
		AggregateValidationStatus aggregatedValidationStatus;
		if (bindings.length == 0) {
			aggregatedValidationStatus =
					new AggregateValidationStatus(
							dbc, AggregateValidationStatus.MAX_SEVERITY);
		} else {
			aggregatedValidationStatus =
					new AggregateValidationStatus(
							toObservableCollection(bindings),
							AggregateValidationStatus.MAX_SEVERITY);
		}
		return aggregatedValidationStatus;
	}

	/**
	 * Returns an observable collection for a given array of validation status
	 * providers.
	 * 
	 * @param observableValue
	 *            the array of observable values
	 * @return an observable collection
	 */
	private static IObservableCollection toObservableCollection(ValidationStatusProvider... validationStatusProviders) {
		WritableList observableCollection = new WritableList();
		for (ValidationStatusProvider validationStatusProvider : validationStatusProviders) {
			observableCollection.add(validationStatusProvider);
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

	/**
	 * Observe and print validation state changes. Utility method to ease bug
	 * tracking.
	 * 
	 * @param label the label to use when printing validation state changes 
	 * @param dbc the databinding context to observe for validation changes
	 */
	public static void observeAndPrintValidationState(final String label, DataBindingContext dbc) {
		AggregateValidationStatus status = new AggregateValidationStatus(dbc, AggregateValidationStatus.MAX_SEVERITY);
		status.addValueChangeListener(new IValueChangeListener() {

			@Override
			public void handleValueChange(ValueChangeEvent event) {
				System.err.println("========================");
				System.err.println(label);
				System.err.println("------------------------");
				System.err.println(event.diff.getNewValue());
				System.err.println("========================");
			}
		});
	}

	/**
	 * Observe and print validation state changes. Utility method to ease bug
	 * tracking in wizards.
	 * 
	 * @param label the label to use when printing validation state changes 
	 * @param dbc the databinding context to observe for validation changes
	 * @param wizardPage the wizard page whose databinding context shall get observed
	 */
	public static void observeAndPrintValidationState(IWizardPage wizardPage, DataBindingContext dbc) {
		observeAndPrintValidationState(wizardPage.getName(), dbc);
	}
	
	/**
	 * Adds the given value change listener to the given observable and removes
	 * it once the given control is disposed.
	 * 
	 * @param listener
	 *            the listener that shall be added
	 * @param observable
	 *            the observable that the listener shall be attached to
	 * @param control
	 *            the control that triggers removal once it's disposed
	 */
	public static void addDisposableValueChangeListener(
			final IValueChangeListener listener, final IObservableValue observable, Control control) {
		Assert.isLegal(listener != null);
		Assert.isLegal(observable != null);
		Assert.isLegal(control != null);
		
		if (control.isDisposed()) {
			return;
		}

		observable.addValueChangeListener(listener);
		control.addDisposeListener(new DisposeListener() {

			@Override
			public void widgetDisposed(DisposeEvent e) {
				observable.removeValueChangeListener(listener);
			}
		});
	}
	
	/**
	 * Disposes the given observable.
	 * 
	 * @param observable the observable that shall be disposed
	 */
	public static void dispose(IObservable observable) {
		if (observable != null) {
			observable.dispose();
		}
	}

	/**
	 * 
	 * Disposes the given validation status provider.
	 * 
	 * @param provider the validation status provider that shall get disposed
	 */
	public static void dispose(ValidationStatusProvider provider) {
		if (isDisposed(provider)) {
			return;
		}
		
		provider.dispose();
	}

	/**
	 * Disposes the given databinding context.
	 * 
	 * @param dbc the databinding context that shall be disposed.
	 */
	public static void dispose(DataBindingContext dbc) {
		if (dbc != null) {
			dbc.dispose();
		}
	}

	/**
	 * Disposes the given validation status providers
	 * @param providers
	 */
	public static void dispose(List<ValidationStatusProvider> providers) {
		if (providers == null
				|| providers.isEmpty()) {
			return;
		}
		for (ValidationStatusProvider provider : providers) {
			dispose(provider);
		}
	}

	/**
	 * Returns {@code true} if the given validation status provider is disposed.
	 * 
	 * @param provider the validation status provider which should be inspected for being disposed.
	 * 
	 * @return true if the given provider is disposed.
	 */
	public static boolean isDisposed(ValidationStatusProvider provider) {
		return provider == null
				|| provider.isDisposed();
	}

	/**
	 * Adds the given status providers to the given data binding context.
	 * 
	 * @param providers the providers to add
	 * @param dbc the context to add to
	 * 
	 * @see ValidationStatusProvider
	 * @see DataBindingContext
	 */
	public static void addValidationStatusProviders(Collection<ValidationStatusProvider> providers, DataBindingContext dbc) {
		for (ValidationStatusProvider provider: new ArrayList<ValidationStatusProvider>(providers)) {
			dbc.addValidationStatusProvider(provider);
		}
	}

	/**
	 * Removes the given status providers from the given data binding context.
	 * 
	 * @param providers the providers to remove
	 * @param dbc the context to remove from
	 * 
	 * @see ValidationStatusProvider
	 * @see DataBindingContext
	 */
	public static void removeValidationStatusProviders(Collection<ValidationStatusProvider> providers, DataBindingContext dbc) {
		for (ValidationStatusProvider provider: new ArrayList<ValidationStatusProvider>(providers)) {
			dbc.removeValidationStatusProvider(provider);
		}
	}
	
	/**
	 * Triggers (model to target) validation of all bindings within the given databinding context. 
	 * 
	 * @param dbc the databinding context
	 * 
	 * @see DataBindingContext 
	 * @see Binding#validateTargetToModel()
	 */
	public static void validateTargetsToModels(DataBindingContext dbc) {
		for (Iterator<?> iterator = dbc.getBindings().iterator(); iterator.hasNext(); ) {
			Binding binding = (Binding) iterator.next();
			binding.validateTargetToModel();
		}
	}
	
	/**
	 * Returns {@code true} if the validation status providers in the given data
	 * binding context are all valid. Returns {@code false} otherwise. 
	 * Disposed validation status providers are not inspected.
	 * 
	 * @param dbc
	 *            the databinding context whose validation status providers will
	 *            get inspected
	 * 
	 * @return true if all validation providers within the databinding context
	 *         are valid
	 * 
	 * @see ValidationStatusProvider
	 * @see DataBindingContext
	 * @see Binding
	 * @see MultiValidator
	 */
	public static boolean isValid(DataBindingContext dbc) {
		if (dbc == null) {
			return false;
		}

		for (Object element : dbc.getValidationStatusProviders()) {
			ValidationStatusProvider validationProvider = (ValidationStatusProvider) element;
			IStatus validationStatus = (IStatus) validationProvider.getValidationStatus().getValue();
			if (!isDisposed(validationProvider)
					&& !validationStatus.isOK()) {
				return false;
			}
		}
		return true;
	}

}
