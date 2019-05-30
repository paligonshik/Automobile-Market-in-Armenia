import pandas as pd
def tree_predict(new):
    import pandas as pd
    from joblib import dump, load
    clf = load('tree.joblib')
    import pickle
    with open("mylist", "rb") as fp:   # Unpickling
        b = pickle.load(fp)
    #new=data[:1]
    #new.drop(columns="sold",inplace=True)
    new['model']=new["brand"]+"_"+new["model"]
    new.drop(columns="brand",inplace=True)
    df_num=new.select_dtypes(exclude="object")## we divide data in this groups to check variance, 
    df_object=new.select_dtypes(include="object")
    #print(df_object)
    ## lets create dummies from object variables and final data
    df_dummies=pd.get_dummies(df_object,prefix=df_object.columns,drop_first=False)
    new=df_num.join(df_dummies)
    for i in b:
        if i not in new.columns:
            new[i]=0
    for i in new.columns:
        if i not in b:
            del new[i]
    class_,probability= clf.predict(new)[0],clf.predict_proba(new)[0][1]
    if class_==0:
        class_="unsold"
    else:
        class_="sold"
    
    print("The probability of the car to be sold is {}, so it is predicted as {}. ".format(probability,class_) )
    return class_,probability
